{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric  #-}
{-# LANGUAGE CPP, BangPatterns #-}
module Wrecker.Recorder where
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import           System.Clock
import           Control.Exception
import           System.Clock.TimeIt
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

data RunResult
  = Success      { resultTime :: !Double
                 , name       :: !String
                 }
  | ErrorStatus  { resultTime :: !Double
                 , errorCode  :: !Int
                 , name       :: !String
                 }
  | Error        { resultTime :: !Double
                 , exception  :: !SomeException
                 , name       :: !String
                 }
  | End
  deriving (Show)

data Event = Event
  { eRunIndex :: !Int
  , eResult   :: !RunResult
  } deriving (Show)

-- | An opaque type for recording actions for profiling.
--   To obtain a 'Recorder' use either 'run', 'defaultMain', 'runOne' or
--   'newStandaloneRecorder'.
data Recorder = Recorder
  { rRunIndex :: !Int
  , rQueue    :: !(TBMQueue Event)
  }

-- The bound here should be configurable
split :: Recorder -> Recorder
split Recorder {..} = Recorder (rRunIndex + 1) rQueue

newRecorder :: Int -> IO Recorder
newRecorder maxSize = Recorder 0 <$> newTBMQueueIO maxSize

stopRecorder :: Recorder -> IO ()
stopRecorder = atomically . closeTBMQueue . rQueue

addEvent :: Recorder -> RunResult -> IO ()
addEvent (Recorder runIndex queue) runResult =
  atomically $ writeTBMQueue queue $ Event runIndex runResult

readEvent :: Recorder -> IO (Maybe Event)
readEvent = atomically . readTBMQueue . rQueue

{- | 'record' is a low level function for collecting timing information.
     Wrap each action of interest in a call to record.

> record recorder $ threadDelay 1000000

  'record' measures the elapsed time of the call, and catches
  'HttpException' in the case of failure. This means failures
   must be thrown if they are to be properly recorded.
-}
record :: forall a. Recorder -> String -> IO a -> IO a
record recorder key action = do
  startTime <- getTime Monotonic

  let recordAction :: IO a
      recordAction = do
        r       <- action
        endTime <- getTime Monotonic
        let !elapsedTime' = diffSeconds endTime startTime
        addEvent recorder $ Success
                            { resultTime = elapsedTime'
                            , name       = key
                            }
        return r

      recordException :: HTTP.HttpException -> IO a
      recordException e = do
        endTime <- getTime Monotonic
        case e of
#if MIN_VERSION_http_client(0,5,0)
          HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _) -> do
            let code = HTTP.statusCode $ HTTP.responseStatus resp
#else
          HTTP.StatusCodeException stat _ _  -> do
            let code = HTTP.statusCode stat
#endif


            addEvent recorder $ ErrorStatus
                                { resultTime = diffSeconds endTime startTime
                                , errorCode  = code
                                , name       = key
                                }

          _ -> addEvent recorder $ Error
                                   { resultTime = diffSeconds endTime startTime
                                   , exception  = toException e
                                   , name       = key
                                   }
        -- rethrow no matter what
        throwIO e

  handle recordException recordAction
