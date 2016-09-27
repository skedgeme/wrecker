{-# LANGUAGE RecordWildCards #-}
module Wrecker.Logger where
import qualified Control.Concurrent.Chan.Unagi.Bounded as U
import System.IO
import System.Timeout
import Data.Foldable (traverse_)
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Function

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError
  deriving (Show, Eq, Ord, Read)

data Logger = Logger
  { thread       :: ThreadId
  , inChan       :: U.InChan (Maybe String)
  , wait         :: IO ()
  , currentLevel :: LogLevel
  }

{- | Create a 'Logger' with the given 'Handle' and max buffer size.
     The logger will drop messages if it is unable to keep up and it's message buffer
     goes over the max size.
-}
newLogger :: Handle
          -- ^ The 'Handle' to log to.
          -> Int
          -- ^ Max buffer size
          -> LogLevel
          -- ^ Minimum log level to log.
          -> IO Logger
newLogger handle maxSize currentLevel = do
  (inChan, outChan) <- U.newChan maxSize
  lock   <- newEmptyMVar
  thread <- readLoop handle outChan lock

  let wait = takeMVar lock

  return Logger {..}

-- | Create a logger using stderr. This is the typical way a logger is created.
newStdErrLogger :: Int -> LogLevel -> IO Logger
newStdErrLogger = newLogger stderr

readLoop :: Handle -> U.OutChan (Maybe String) -> MVar () -> IO ThreadId
readLoop handle chan lock = forkIO $ do
  fix $ \next -> do
    -- Block on the next elemen
    -- If it "Just" print it and loop
    -- Otherwise we are not with the loop
    U.readChan chan >>= traverse_ (\msg -> do
        hPutStrLn handle msg
        next
      )

  putMVar lock ()

-- True if the write was successful or False otherwise
writeLogger :: Logger -> LogLevel -> String -> IO Bool
writeLogger Logger {..} messageLevel msg =
  if (currentLevel <= messageLevel) then
    U.tryWriteChan inChan $ Just msg
  else
    return False


shutdownLogger :: Int -> Logger -> IO ()
shutdownLogger waitTime logger@(Logger {..}) = do
  U.writeChan inChan Nothing
  mtimedOut <- timeout waitTime wait
  case mtimedOut of
    Nothing -> forceShutdownLogger logger
    Just () -> return ()

forceShutdownLogger :: Logger -> IO ()
forceShutdownLogger Logger {..} = killThread thread

logDebug :: Logger -> String -> IO Bool
logDebug logger = writeLogger logger LevelDebug

logInfo :: Logger -> String -> IO Bool
logInfo logger = writeLogger logger LevelInfo

logWarn :: Logger -> String -> IO Bool
logWarn logger = writeLogger logger LevelWarn

logError :: Logger -> String -> IO Bool
logError logger = writeLogger logger LevelError
