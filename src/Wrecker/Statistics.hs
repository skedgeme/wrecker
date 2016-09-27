{-# LANGUAGE RecordWildCards, BangPatterns, LambdaCase, OverloadedStrings #-}
module Wrecker.Statistics where
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Wrecker.Recorder
import Wrecker.Options
import qualified Text.Tabular.AsciiArt as AsciiArt
import Text.Tabular
import Text.Printf
import System.Console.Ansigraph.Core
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Data.Aeson (object, ToJSON (..), (.=), Value (..))
import Data.List (sortBy)
import Data.Function

data Histogram = Histogram
  deriving (Show, Eq, Ord)

data VarianceAndMean = VarianceAndMean
  { var         :: {-# UNPACK #-} !Double
  , varMeanDiff :: {-# UNPACK #-} !Double
  , varMean     :: {-# UNPACK #-} !Double
  , varCount    :: {-# UNPACK #-} !Double
  } deriving (Show, Eq, Ord)

emptyVarianceAndMean :: VarianceAndMean
emptyVarianceAndMean = VarianceAndMean
  { var         = 0
  , varMeanDiff = 0
  , varMean     = 0
  , varCount    = 0
  }

stableVarianceStep :: VarianceAndMean -> Double -> VarianceAndMean
stableVarianceStep VarianceAndMean {..} !newValue =
  let !newCount     = varCount + 1
      !newMean      = varMean + ((newValue - varMean) / newCount)
      !newMeanDiff  = varMeanDiff + ((newValue - varMean)*(newValue - newMean))
  in VarianceAndMean (newMeanDiff / newCount) newMeanDiff newMean newCount

insertHist :: Histogram -> Double -> Histogram
insertHist h _ = h

-- | These are the
data Statistics = Statistics
  { sVarMean   :: !VarianceAndMean
  -- ^ Combined variance and mean. This type contains information useful for
  --   incremental computation of the variance and mean. To get the individual
  --   components use 'variance' and 'mean'.
  , sMax       :: !Double
  -- ^ The maximum time
  , sMin       :: !Double
  -- ^ The maximum time
  , sHistogram :: !Histogram
  -- ^ A histogram of times
  , sTotal     :: !Double
  -- ^ The total time
  } deriving (Show, Eq, Ord)

-- | Extract the mean
mean :: Statistics -> Double
mean = varMean . sVarMean

-- | Extract the variance
variance :: Statistics -> Double
variance = var . sVarMean

statsCount :: Statistics -> Int
statsCount = floor . (+ 0.1) . varCount . sVarMean

emptyStatistics :: Statistics
emptyStatistics = Statistics
  { sVarMean   = emptyVarianceAndMean
  , sMax       = 0
  , sMin       = 1e32 -- i don't know ... maxBound won't work
  , sHistogram = Histogram
  , sTotal     = 0
  }

stepStatistics :: Statistics -> Double -> Statistics
stepStatistics stats value = stats
     { sVarMean   = stableVarianceStep (sVarMean stats) value
     , sMax       = max (sMax stats) value
     , sMin       = min (sMin stats) value
     , sHistogram = insertHist (sHistogram stats) value
     , sTotal     = sTotal stats + value
     }

{- | This type includes statistics for all of the result values we can detect.
     This type is used by AllStats to compute per key (URL) statistics among
     other uses.
-}
data ResultStatistics = ResultStatistics
  { rs2xx    :: !Statistics
  , rs4xx    :: !Statistics
  , rs5xx    :: !Statistics
  , rsFailed :: !Statistics
  , rsRollup :: !Statistics
  } deriving (Show, Eq, Ord)

emptyResultStatistics :: ResultStatistics
emptyResultStatistics = ResultStatistics
  { rs2xx    = emptyStatistics
  , rs4xx    = emptyStatistics
  , rs5xx    = emptyStatistics
  , rsFailed = emptyStatistics
  , rsRollup = emptyStatistics
  }

stepResultStatistics :: ResultStatistics -> RunResult -> ResultStatistics
stepResultStatistics stats = \case
  Success      { .. } -> stats { rs2xx    = stepStatistics (rs2xx    stats)
                                                           resultTime
                               , rsRollup = stepStatistics (rsRollup stats)
                                                           resultTime
                               }
  ErrorStatus  { .. }
    | is4xx errorCode -> stats { rs4xx    = stepStatistics (rs4xx    stats)
                                                           resultTime
                               , rsRollup = stepStatistics (rsRollup stats)
                                                           resultTime
                               }
    | otherwise       -> stats { rs5xx    = stepStatistics (rs5xx    stats)
                                                           resultTime
                               , rsRollup = stepStatistics (rsRollup stats)
                                                           resultTime
                               }
  Error        { .. } -> stats { rsFailed = stepStatistics (rsFailed stats)
                                                           resultTime
                               , rsRollup = stepStatistics (rsRollup stats)
                                                           resultTime
                               }
  End                 -> stats

count2xx :: ResultStatistics -> Int
count2xx = statsCount . rs2xx

count4xx :: ResultStatistics -> Int
count4xx = statsCount . rs4xx

count5xx :: ResultStatistics -> Int
count5xx = statsCount . rs5xx

countFailed :: ResultStatistics -> Int
countFailed = statsCount . rsFailed

errorRate :: ResultStatistics -> Double
errorRate x
  = fromIntegral (count4xx x + count5xx x + countFailed x)
  / fromIntegral (count2xx x + count4xx x + count5xx x + countFailed x)

isEntirelySuccessful :: ResultStatistics -> Bool
isEntirelySuccessful x = (count4xx x + count5xx x + countFailed x) == 0

successfulToResult :: Statistics -> ResultStatistics
successfulToResult x = emptyResultStatistics { rs2xx = x }

{- | AllStats has all of the ... stats. This type stores all of the information
     'wrecker' uses to display metrics to the user.
-}
data AllStats = AllStats
  { aRollup       :: !ResultStatistics
  -- ^ The "total" stats. This computes things like total 2xx and average time
  --   Across all requests.
  , aCompleteRuns :: !ResultStatistics
  -- ^ This contains statistic for actions that completed entirely successfully.
  --   Useful for knowing if a complex action is under some desired total time.
  , aRuns         :: !(HashMap Int    ResultStatistics)
  -- ^ This is an intermediate holding spot for scripts that are still executing.
  , aPerUrl       :: !(HashMap String ResultStatistics)
  -- ^ This is the per key (URL) statistics.
  } deriving (Show, Eq)

emptyAllStats :: AllStats
emptyAllStats = AllStats
  { aRollup       = emptyResultStatistics
  , aCompleteRuns = emptyResultStatistics
  , aRuns         = H.empty
  , aPerUrl       = H.empty
  }

is4xx :: Int -> Bool
is4xx x = x > 399 && x < 500

stepAllStats :: AllStats -> Int -> String -> RunResult -> AllStats
stepAllStats allStats index key result =
  case result of
    End ->  let mRunStats = H.lookup index $ aRuns allStats
            in case mRunStats of
              Nothing -> allStats
              Just stats
                 | errorRate stats == 0 ->
                    let runTime = sTotal $ rs2xx stats
                    in allStats { aCompleteRuns = stepResultStatistics
                                                    (aCompleteRuns allStats)
                                                    (Success runTime "")
                                , aRuns         = H.delete index
                                                $ aRuns allStats
                                }
                 | otherwise -> allStats { aRuns = H.delete index
                                                 $ aRuns allStats
                                         }
    _   -> allStats
            { aRollup = stepResultStatistics (aRollup allStats) result
            , aRuns   = H.insertWith (\_ x -> stepResultStatistics x result)
                                     index
                                     (stepResultStatistics
                                       emptyResultStatistics
                                       result
                                     )
                                     $ aRuns allStats
            , aPerUrl = H.insertWith (\_ x -> stepResultStatistics x result)
                                     key
                                     (stepResultStatistics
                                       emptyResultStatistics
                                       result
                                     )
                      $ aPerUrl allStats
            }
-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------
renderHistogram :: U.Vector Int -> String
renderHistogram bins = renderPV $ U.toList powers where
  total  = fromIntegral $ U.sum bins
  powers = U.map (\x -> fromIntegral x / total) bins

statToRow :: ResultStatistics -> [String]
statToRow x
  = [ printf "%.4f" $ mean     $ rs2xx x
    , printf "%.8f" $ variance $ rs2xx x
    , printf "%.4f" $ sMax     $ rs2xx x
    , printf "%.4f" $ sMin     $ rs2xx x
    , show $ count2xx x
    , show $ count4xx x
    , show $ count5xx x
    , show $ countFailed x
    ,  printf "%.4f" $ errorRate x
    , renderHistogram $ mempty
    ]

pprStats :: Maybe Int -> AllStats -> String
pprStats nameSize stats = AsciiArt.render id id id $ statsTable nameSize stats

statsTable ::  Maybe Int -> AllStats -> Table String String String
statsTable nameSize AllStats {..}
  = let sortedPerUrl = sortBy (compare `on` fst) $ H.toList aPerUrl
  in Table (Group SingleLine
          $ map (Header . maybe id take nameSize . fst) sortedPerUrl
          )
          (Group SingleLine [ Header "mean"
                            , Header "variance"
                            , Header "max"
                            , Header "min"
                            , Header "Successful Count"
                            , Header "4xx Count"
                            , Header "5xx Count"
                            , Header "Failure Count"
                            , Header "Error Rate"
                            , Header "Histogram"
                            ]
          )
          (map (statToRow . snd) sortedPerUrl)
  +====+ SemiTable (Group SingleLine [Header "All"]) (statToRow aRollup)
  +====+ SemiTable (Group SingleLine [Header "Successful Runs"])
                   (statToRow aCompleteRuns)

printStats :: Options -> AllStats -> IO ()
printStats options sampler
  = putStrLn $ pprStats (requestNameColumnSize options) sampler
------------------------------------------------------------------------------
-- JSON Serialization
------------------------------------------------------------------------------
instance ToJSON Statistics where
  toJSON x = object
    [ "mean"     .= mean       x
    , "variance" .= variance   x
    , "max"      .= sMax       x
    , "min"      .= sMin       x
    , "total"    .= sTotal     x
    , "count"    .= statsCount x
    ]

instance ToJSON ResultStatistics where
  toJSON ResultStatistics {..} = object
    [ "2xx"    .= rs2xx
    , "4xx"    .= rs4xx
    , "5xx"    .= rs5xx
    , "failed" .= rsFailed
    , "rollup" .= rsRollup
    ]

instance ToJSON AllStats where
  toJSON AllStats {..} = object
    [ "per-request" .= Object
                     ( H.fromList
                     $ map (\(k, v) -> (T.pack k, toJSON v))
                     $ H.toList aPerUrl
                     )
    , "runs"        .= aCompleteRuns
    , "rollup"      .= aRollup
    ]
