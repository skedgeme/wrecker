{-# LANGUAGE RecordWildCards, CPP #-}
module Wrecker.Options where
import Options.Applicative.Builder
import Options.Applicative
import Control.Exception
import Wrecker.Logger
#if __GLASGOW_HASKELL__ < 800
import Data.Monoid
#endif

data RunType = RunCount Int | RunTimed Int
  deriving (Show, Eq)
  
data DisplayMode = Interactive | NonInteractive 
  deriving (Show, Eq, Read)
  
data Options = Options
  { concurrency           :: Int
  -- ^ The number of simulatanous connections
  , binCount              :: Int
  -- ^ The number of bins for the histogram
  , runStyle              :: RunType
  -- ^ runStyle determines if the 'wrecker' runs for a specified
  --   time period or for a specified number of runs.
  , timeoutTime           :: Int
  -- ^ How long to wait after the first benchmark for the other threads 
  --   to finish
  , displayMode           :: DisplayMode
  -- ^ This controls the command line display. It can be either Interactive
  --   of NonInteractive
  , logLevel              :: LogLevel
  -- ^ 
  , match                 :: String
  -- ^ Set this to filter the benchmarks using a pattern
  , requestNameColumnSize :: Maybe Int
  -- ^ Limit the request name column to the given size
  , outputFilePath        :: Maybe FilePath
  -- ^ Dump the results to JSON file
  , silent                :: Bool
  -- ^ Set 'silent' to true to disable all output.
  } deriving (Show, Eq)

-- | 'defaultOptions' provides sensible default for the 'Options' 
--   types
defaultOptions :: Options
defaultOptions = Options 
  { concurrency           = 1
  , binCount              = 20
  , runStyle              = RunCount 1
  , timeoutTime           = 100000000
  , displayMode           = NonInteractive
  , logLevel              = LevelError
  , match                 = ""
  , requestNameColumnSize = Nothing
  , outputFilePath        = Nothing
  , silent                = False
  } 

data PartialOptions = PartialOptions 
  { mConcurrency           :: Maybe Int
  , mBinCount              :: Maybe Int
  , mRunStyle              :: Maybe RunType
  , mTimeoutTime           :: Maybe Int
  , mDisplayMode           :: Maybe DisplayMode
  , mLogLevel              :: Maybe LogLevel
  , mMatch                 :: Maybe String
  , mRequestNameColumnSize :: Maybe Int
  , mOutputFilePath        :: Maybe FilePath
  , mSilent                :: Maybe Bool
  } deriving (Show, Eq)
  
instance Monoid PartialOptions where
  mempty = PartialOptions 
            { mConcurrency           = Just $ concurrency           defaultOptions 
            , mBinCount              = Just $ binCount              defaultOptions
            , mRunStyle              = Just $ runStyle              defaultOptions
            , mTimeoutTime           = Just $ timeoutTime           defaultOptions
            , mDisplayMode           = Just $ displayMode           defaultOptions
            , mLogLevel              = Just $ logLevel              defaultOptions
            , mMatch                 = Just $ match                 defaultOptions
            , mRequestNameColumnSize = requestNameColumnSize        defaultOptions
            , mOutputFilePath        = outputFilePath               defaultOptions
            , mSilent                = Just $ silent                defaultOptions
            }
  mappend x y = PartialOptions 
                  { mConcurrency           =  mConcurrency x <|> mConcurrency y
                  , mBinCount              =  mBinCount    x <|> mBinCount    y
                  , mRunStyle              =  mRunStyle    x <|> mRunStyle    y
                  , mTimeoutTime           =  mTimeoutTime x <|> mTimeoutTime y
                  , mDisplayMode           =  mDisplayMode x <|> mDisplayMode y
                  , mLogLevel              =  mLogLevel    x <|> mLogLevel    y
                  , mMatch                 =  mMatch       x <|> mMatch       y
                  , mRequestNameColumnSize =  mRequestNameColumnSize x 
                                          <|> mRequestNameColumnSize y
                  , mOutputFilePath        =  mOutputFilePath x 
                                          <|> mOutputFilePath y
                  , mSilent                =  mSilent      x <|> mSilent      y
                  }

completeOptions :: PartialOptions -> Maybe Options
completeOptions options = 
  case options <> mempty of
    PartialOptions 
      { mConcurrency           = Just concurrency
      , mBinCount              = Just binCount
      , mRunStyle              = Just runStyle
      , mTimeoutTime           = Just timeoutTime
      , mDisplayMode           = Just displayMode
      , mLogLevel              = Just logLevel
      , mMatch                 = Just match
      , mRequestNameColumnSize = requestNameColumnSize
      , mOutputFilePath        = outputFilePath
      , mSilent                = Just silent
      } -> Just $ Options {..}
    _ -> Nothing 
  
optionalOption :: Read a => Mod OptionFields a -> Parser (Maybe a)
optionalOption = optional . option auto

optionalStrOption :: Mod OptionFields String -> Parser (Maybe String)
optionalStrOption = optional . strOption

optionalSwitch :: Mod FlagFields Bool -> Parser (Maybe Bool)
optionalSwitch = optional . switch

pPartialOptions :: Parser PartialOptions 
pPartialOptions
   =  PartialOptions
  <$> optionalOption 
      (  long "concurrency"
      <> help "Number of threads for concurrent requests"
      )
  <*> optionalOption 
      (  long "bin-count"
      <> help "Number of bins for latency histogram"
      )
  <*> optional 
      (  RunCount <$> option auto
                   (  long "run-count"
                  <>  help "number of times to repeat "
                   )
     <|> RunTimed <$> option auto
                   (  long "run-timed"
                  <>  help "number of seconds to repeat "
                   )
      )
  <*> optionalOption
      (  long "timeout-time"
      <> help "How long to wait for all requests to finish"
      )
  <*> optionalOption 
      (  long "display-mode"
      <> help "Display results interactively"
      )
  <*> optionalOption 
      (  long "log-level"
      <> help "Display results interactively"
      )
  <*> optionalStrOption
      (  long "match"
      <> help "Only run tests that match the glob"
      )
  <*> optionalOption 
      (  long "request-name-size"
      <> help "Request name size for the terminal display"
      )
  <*> optionalStrOption 
      (  long "output-path"
      <> help "Save a JSON file of the the statistics to given path"
      )
  <*> optionalSwitch 
      (  long "silent"
      <> help "Disable all output"
      )

runParser :: IO Options
runParser = do 
  let opts = info (helper <*> pPartialOptions)
               ( fullDesc
               <> progDesc "Welcome to wrecker"
               <> header "wrecker - HTTP stress tester and benchmarker" 
               )
  
  partialOptions <- execParser opts 
  case completeOptions partialOptions of
    Nothing -> throwIO $ userError ""
    Just x  -> return x
  