{-# LANGUAGE RecordWildCards, ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Wrecker.Recorder where
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import           System.Clock
import           Control.Exception
import           System.Clock.TimeIt

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
  , result    :: !RunResult
  } deriving (Show)

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

record :: forall a. Recorder -> String -> IO a -> IO a
record recorder key action = do
  startTime <- getTime Monotonic
  
  let recordAction :: IO a
      recordAction = do
        r       <- action
        endTime <- getTime Monotonic
        addEvent recorder $ Success 
                            { resultTime = diffTime endTime startTime
                            , name       = key
                            }
        return r
  
      recordException :: HTTP.HttpException -> IO a
      recordException e = do
        endTime <- getTime Monotonic
        case e of 
          HTTP.StatusCodeException stat _ _  -> do
            let code = HTTP.statusCode stat
            addEvent recorder $ ErrorStatus
                                { resultTime = diffTime endTime startTime 
                                , errorCode  = code 
                                , name       = key
                                }
                                      
          _ -> addEvent recorder $ Error 
                                   { resultTime = diffTime endTime startTime
                                   , exception  = toException e
                                   , name       = key
                                   }
        -- rethrow no matter what
        throwIO e
  
  handle recordException recordAction
