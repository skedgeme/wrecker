{-# LANGUAGE RecordWildCards, ScopedTypeVariables, LambdaCase, BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
module Wrecker.Runner where
import Wrecker.Logger
import System.Posix.Signals
import Data.Function
import qualified Control.Concurrent.Thread.BoundedThreadGroup as BoundedThreadGroup
import Control.Concurrent
import Control.Exception
import Wrecker.Recorder
import Wrecker.Statistics
import Control.Monad
import Data.Foldable (for_)
import System.Timeout
import qualified Graphics.Vty            as VTY
import Control.Concurrent.NextRef
import Wrecker.Options
import Data.IORef
import System.IO
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.List (isInfixOf)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import qualified Control.Immortal as Immortal
-- TODO configure whether errors are used in times or not

newStandaloneRecorder :: IO (NextRef AllStats, Immortal.Thread, Recorder)
newStandaloneRecorder = do
  recorder      <- newRecorder 10000
  logger        <- newStdErrLogger 1000 LevelError 
  (ref, thread) <- sinkRecorder logger recorder
  return (ref, thread, recorder)

sinkRecorder :: Logger -> Recorder -> IO (NextRef AllStats, Immortal.Thread)
sinkRecorder logger recorder = do 
  ref <- newNextRef emptyAllStats
  
  immortal <- Immortal.createWithLabel "collectEvent" 
            $ \_ -> collectEvent logger ref recorder
  
  return (ref, immortal)

updateSampler :: NextRef AllStats -> Event -> IO AllStats
updateSampler !ref !event = modifyNextRef ref
                        $ \x -> let !new = stepAllStats x
                                                        (eRunIndex event)
                                                        (name $ result event)
                                                        (result        event)
                                in (new, new)

collectEvent :: Logger -> NextRef AllStats -> Recorder -> IO ()
collectEvent logger ref recorder = fix $ \next -> do
  mevent <- readEvent recorder
  for_ mevent $ \event -> do
    sampler <- updateSampler ref event
    logDebug logger $ show sampler
    next

runAction :: Logger
          -> Int
          -> Int
          -> RunType
          -> (Recorder -> IO ())
          -> Recorder -> IO ()
runAction logger timeoutTime concurrency runStyle action recorder = do
  threadLimit <- BoundedThreadGroup.new concurrency
  recorderRef <- newIORef recorder

  let takeRecorder = atomicModifyIORef' recorderRef $ \x -> (split x, x)
      actionThread = void
                   $ BoundedThreadGroup.forkIO threadLimit $ do
                     rec <- takeRecorder
                     handle (\(e :: SomeException) -> do
                              logWarn logger $ show e
                              addEvent recorder $ Error
                                                   { resultTime = 0
                                                   , exception  = e
                                                   , name       = "__UNKNOWN__"
                                                   }
                              addEvent rec End
                            )
                            $ do
                                action rec
                                addEvent rec End


  case runStyle of
    RunCount count -> replicateM_ (count * concurrency) actionThread
    RunTimed time  -> void $ timeout time $ forever actionThread

  mtimeout <- timeout timeoutTime $ BoundedThreadGroup.wait threadLimit
  case mtimeout of
    Nothing -> void
             $ logError logger
             $ "Timed out waiting for all "
             ++ "threads to complete"
    Just ()  -> return ()

------------------------------------------------------------------------------
---   Generic Run Function
-------------------------------------------------------------------------------
runWithNextVar :: Options
              -> (NextRef AllStats -> IO ())
              -> (NextRef AllStats -> IO ())
              -> (Recorder -> IO ())
              -> IO AllStats
runWithNextVar (Options {..}) consumer final action = do
  recorder <- newRecorder 100000
  sampler  <- newNextRef emptyAllStats
  logger   <- newStdErrLogger 100000 logLevel
  -- Collect events and
  forkIO $ handle (\(e :: SomeException) -> void $ logError logger $ show e)
         $ collectEvent logger sampler recorder

  consumer sampler

  logDebug logger "Starting Runs"
  runAction logger timeoutTime concurrency runStyle action recorder `finally`
    (do logDebug logger "Shutting Down"
        stopRecorder recorder
        shutdownLogger 1000000 logger
        final sampler
    )
  readLast sampler
-------------------------------------------------------------------------------
---   Non-interactive Rendering
-------------------------------------------------------------------------------
printLastSamples :: Options -> NextRef AllStats -> IO ()
printLastSamples options sampler = printStats options =<< readLast sampler

runNonInteractive :: Options -> (Recorder -> IO ()) -> IO AllStats
runNonInteractive options action = do
  let shutdown sampler = do
        putStrLn ""
        hFlush stdout
        hSetBuffering stdout (BlockBuffering (Just 100000000))
        printLastSamples options sampler
        hFlush stdout
        for_ (outputFilePath options) $ \filePath ->
          BSL.writeFile filePath . encode =<< readLast sampler

  runWithNextVar options (const $ return ()) shutdown action
-------------------------------------------------------------------------------
---   Interactive Rendering
-------------------------------------------------------------------------------
printLoop :: Options
          -> VTY.DisplayContext
          -> VTY.Vty
          -> NextRef AllStats
          -> IO ()
printLoop options context vty sampler
  = fix $ \next -> takeNextRef sampler >>= \case
      Nothing      -> return ()
      Just allStats -> do
        updateUI (requestNameColumnSize options) context allStats
        VTY.refresh vty
        next

processInputForCtrlC :: TChan VTY.Event -> IO ThreadId
processInputForCtrlC chan = forkIO $ forever $ do
  event <- atomically $ readTChan chan
  case event of
    VTY.EvKey (VTY.KChar 'c') [VTY.MCtrl] -> raiseSignal sigINT
    _ -> return ()

updateUI :: Maybe Int -> VTY.DisplayContext ->  AllStats -> IO ()
updateUI nameSize displayContext stats
  = VTY.outputPicture displayContext
  $ VTY.picForImage
  $ VTY.vertCat
  $ map (VTY.string VTY.defAttr)
  $ lines
  $ pprStats nameSize stats

runInteractive :: Options -> (Recorder -> IO ()) -> IO AllStats
runInteractive options action = do
  vtyConfig       <- VTY.standardIOConfig
  vty             <- VTY.mkVty vtyConfig
  let output       = VTY.outputIface vty
  (width, height) <- VTY.displayBounds output
  displayContext  <- VTY.displayContext output (width, height)
  inputThread     <- processInputForCtrlC $ VTY._eventChannel $ VTY.inputIface vty

  let shutdown sampler = do
          killThread inputThread
          VTY.shutdown vty
          putStrLn ""
          hSetBuffering stdout (BlockBuffering (Just 100000000))
          printLastSamples options sampler
          hFlush stdout
          for_ (outputFilePath options) $ \filePath ->
            BSL.writeFile filePath . encode =<< readLast sampler

  runWithNextVar options (\sampler -> void
                                    $ forkIO (printLoop options
                                                        displayContext
                                                        vty
                                                        sampler
                                             )
                         )
                         shutdown
                         action
-------------------------------------------------------------------------------
---   Main Entry Point
-------------------------------------------------------------------------------
-- | 'run' is the a lower level entry point, compared to 'defaultMain'. Unlike
--    'defaultMain' no command line argument parsing is performed. Instead,
--    'Options' are directly passed in. 'defaultOptions' can be used as a
--    default argument for 'run'.
--
--    Like 'defaultMain', 'run' creates a 'Recorder' and passes it each
--    benchmark.
run :: Options -> [(String, Recorder -> IO ())] -> IO (HashMap String AllStats)
run options actions = do
  hSetBuffering stderr LineBuffering

  fmap H.fromList . forM actions $ \(groupName, action) -> do
    if (match options `isInfixOf` groupName) then do
      putStrLn groupName
      (groupName, ) <$> case displayMode options of  
        NonInteractive -> runNonInteractive options action
        Interactive    -> runInteractive    options action
    else
      return (groupName, emptyAllStats)
