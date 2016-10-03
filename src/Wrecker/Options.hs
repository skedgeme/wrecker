{-# LANGUAGE RecordWildCards, CPP #-}
module Wrecker.Options where
import Options.Applicative.Builder
import Options.Applicative
import Control.Exception
import Wrecker.Logger
import Data.Monoid

{- | There are two typical ways to invoke 'wrecker'. 'RunCount' will execute
     each a script 'n' times, where 'n' is the parameter for 'RunCount'.
     Alternatively, 'wrecker' can run for specified time with 'RunTimed'.
-}
data RunType = RunCount Int | RunTimed Int
  deriving (Show, Eq)

{- | 'DisplayMode' controls how results are displayed in the console. The
default is 'NonInterative' which returns the final results at the end of the
program. 'Interactive' will show partial results as the program updates.
-}
data DisplayMode = Interactive | NonInteractive
  deriving (Show, Eq, Read)

data URLDisplay = Path | Full
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
  , urlDisplay              :: URLDisplay
  } deriving (Show, Eq)

-- | 'defaultOptions' provides sensible default for the 'Options'
--   types
defaultOptions :: Options
defaultOptions = Options
  { concurrency           = 10
  , binCount              = 20
  , runStyle              = RunTimed 10
  , timeoutTime           = 100000000
  , displayMode           = NonInteractive
  , logLevel              = LevelError
  , match                 = ""
  , requestNameColumnSize = Nothing
  , outputFilePath        = Nothing
  , silent                = False
  , urlDisplay              = Path
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
  , murlDisplay            :: Maybe URLDisplay
  } deriving (Show, Eq)

instance Monoid PartialOptions where
  mempty = PartialOptions
            { mConcurrency           = Just $ concurrency    defaultOptions
            , mBinCount              = Just $ binCount       defaultOptions
            , mRunStyle              = Just $ runStyle       defaultOptions
            , mTimeoutTime           = Just $ timeoutTime    defaultOptions
            , mDisplayMode           = Just $ displayMode    defaultOptions
            , mLogLevel              = Just $ logLevel       defaultOptions
            , mMatch                 = Just $ match          defaultOptions
            , mRequestNameColumnSize = requestNameColumnSize defaultOptions
            , mOutputFilePath        = outputFilePath        defaultOptions
            , mSilent                = Just $ silent         defaultOptions
            , murlDisplay              = Just $ urlDisplay       defaultOptions
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
                  , murlDisplay            =  murlDisplay  x <|> murlDisplay  y
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
      , murlDisplay              = Just urlDisplay
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
  <*> optional
      (  Interactive    <$ switch (long "interactive")
     <|> NonInteractive <$ switch (long "non-interactive")
      )
  <*> optionalOption
      (  long "log-level"
      <> help "Log to stderr events of criticality greater than the LOG_LEVEL"
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
  <*> optional
      (  Path <$ switch (long "relative-url-display")
     <|> Full <$ switch (long "absolute-url-display")
      )
{- | Run the command line parse and return the 'Options'

'runParser' can parse the following options

> $ wrecker-based-app --help
>
> wrecker - HTTP stress tester and benchmarker
>
> Usage: example [--concurrency ARG] [--bin-count ARG] ([--run-count ARG] |
>                [--run-timed ARG]) [--timeout-time ARG] [--display-mode ARG]
>                [--log-level ARG] [--match ARG] [--request-name-size ARG]
>                [--output-path ARG] [--silent]
>  Welcome to wrecker
>
> Available options:
>  -h,--help                Show this help text
>  --concurrency ARG        Number of threads for concurrent requests
>  --bin-count ARG          Number of bins for latency histogram
>  --run-count ARG          number of times to repeat
>  --run-timed ARG          number of seconds to repeat
>  --timeout-time ARG       How long to wait for all requests to finish
>  --display-mode ARG       Display results interactively
>  --log-level ARG          Display results interactively
>  --match ARG              Only run tests that match the glob
>  --request-name-size ARG  Request name size for the terminal display
>  --output-path ARG        Save a JSON file of the the statistics to given path
>  --silent                 Disable all output
-}
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
