{-# LANGUAGE QuasiQuotes, LambdaCase, RecordWildCards #-}
module WreckerSpec where
import Wrecker
import qualified Wrecker.Statistics as Stats
import Test.Hspec
import qualified Server as Server
import qualified Client as Client
import Network.Wreq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Distribution (Distribution)
import qualified Distribution as Distribution
import Control.Concurrent
import Network.Wai.Handler.Warp (Port)
import Data.Maybe (fromJust)
import qualified Statistics.Sample as S
import Control.Monad (replicateM)
import System.Clock.TimeIt
import qualified Data.Vector.Unboxed as U
import Control.Concurrent.NextRef (NextRef)
import qualified Control.Concurrent.NextRef as NextRef

sleepAmount :: Int  -- microseconds
sleepAmount = 10000 -- 10 milliseconds
              -- 0
              -- 1 millisecond 
              
data Gaussian = Gaussian 
                { mean     :: Double
                , variance :: Double
                } deriving (Show, Eq)
                
subtractGaussian :: Gaussian -> Gaussian -> Gaussian
subtractGaussian x y 
  = Gaussian (mean     x - mean     y) 
             (variance x - variance y)

gaussianToDistribution :: Gaussian -> Distribution
gaussianToDistribution Gaussian {..} = Distribution.Gaussian {..}

urlStatsToDist :: HashMap String ResultStatistics 
               -> Server.Root Gaussian
urlStatsToDist stats = 
  let gaussians = fmap toDist stats
      Just root            = H.lookup "root"     gaussians
      Just products        = H.lookup "products" gaussians
      Just login           = H.lookup "login"    gaussians
      Just usersIndex      = H.lookup "user"     gaussians
      Just cartsIndex      = H.lookup "cart"     gaussians
      Just cartsIndexItems = H.lookup "items"    gaussians
      Just checkout        = H.lookup "checkout" gaussians
                                                 
  in Server.Root {..}

substractDist :: Server.Root Gaussian 
              -> Server.Root Gaussian
              -> Server.Root Gaussian
substractDist x y = Server.Root 
  { Server.root            = subtractGaussian (Server.root            x) (Server.root            y)           
  , Server.products        = subtractGaussian (Server.products        x) (Server.products        y)
  , Server.login           = subtractGaussian (Server.login           x) (Server.login           y)
  , Server.usersIndex      = subtractGaussian (Server.usersIndex      x) (Server.usersIndex      y)
  , Server.cartsIndex      = subtractGaussian (Server.cartsIndex      x) (Server.cartsIndex      y)
  , Server.cartsIndexItems = subtractGaussian (Server.cartsIndexItems x) (Server.cartsIndexItems y)
  , Server.checkout        = subtractGaussian (Server.checkout        x) (Server.checkout        y)
  }

-- Create a distribution for sleeping
rootDistribution :: Server.Root Gaussian
rootDistribution = pure $ Gaussian (fromIntegral sleepAmount / 1e6 ) 0

main :: IO ()
main = hspec spec

toDist :: ResultStatistics -> Gaussian
toDist x 
  = let stats = rs2xx x 
  in Gaussian (Stats.mean stats) (Stats.variance stats)


class Approx a where
  approx :: a -> a -> Bool

instance Approx Double where
  approx x y = abs (x - y) < 0.001

instance Approx Gaussian where
  approx x y = approx (mean     x) (mean     y)
            && approx (variance x) (variance y)

instance Approx a => Approx (Server.Root a) where
  approx x y = approx (Server.root            x) (Server.root            y)
            && approx (Server.products        x) (Server.products        y)
            && approx (Server.cartsIndex      x) (Server.cartsIndex      y)
            && approx (Server.cartsIndexItems x) (Server.cartsIndexItems y)
            && approx (Server.usersIndex      x) (Server.usersIndex      y)
            && approx (Server.checkout        x) (Server.checkout        y)
            && approx (Server.login           x) (Server.login           y)
            

runWrecker :: Int -> IO AllStats
runWrecker port 
   =  let key = "key" 
   in fromJust 
   .  H.lookup key 
  <$> Wrecker.run (defaultOptions 
                    { runStyle    = RunCount 1000
                    , displayMode = Interactive
                    }
                  )
                  [ (key, Client.testScript port)
                  ]

timeThreadDelay :: Int -> Int -> IO (Double, Double)
timeThreadDelay sleepTime count 
   = fmap (S.meanVariance . U.fromList)
   $ replicateM count 
   $ fmap snd
   $ elapsedTime 
   $ threadDelay sleepTime

calculateOverhead :: IO (Server.Root Gaussian, Server.Root Gaussian, Gaussian)
calculateOverhead = do 
  
  putStrLn "computing threadDelay dist"
  (threadSleepMean, threadSleepVariance) <- timeThreadDelay sleepAmount 10
  putStrLn $ "threadSleepMean = "     ++ show threadSleepMean
  putStrLn $ "threadSleepVariance = " ++ show threadSleepVariance 
  
  (port, _, threadId, ref) <- Server.run Server.zeroDistribution
  
  allStats    <- runWrecker port
  serverStats <- NextRef.readLast ref
  
  putStrLn $ Stats.pprStats Nothing serverStats
  
  killThread threadId
  
  return ( urlStatsToDist $ aPerUrl allStats
         , urlStatsToDist $ aPerUrl serverStats
         , Gaussian threadSleepMean threadSleepVariance
         )
  
start :: IO (Port, ThreadId, Server.Root Gaussian, Gaussian, NextRef AllStats)
start = do
  (overhead, serverTimes, expected) <- calculateOverhead
  (port, _, threadId, ref) <- Server.run (fmap gaussianToDistribution rootDistribution)
  
  putStrLn $ "serverTimes" ++ show serverTimes
  putStrLn $ "overhead"    ++ show overhead  
  
  return (port, threadId, substractDist overhead serverTimes, expected, ref)
  
stop :: (Port, ThreadId, Server.Root Gaussian, Gaussian, NextRef AllStats) -> IO ()
stop (_, threadId, _, _, _) = killThread threadId

shouldBeApprox :: (Show a, Approx a) => a -> a -> IO ()
shouldBeApprox x y = shouldSatisfy (x, y) (uncurry approx)

spec :: Spec
spec = beforeAll start
     $ afterAll  stop
     $ describe "Wrecker"
     $ it "measure requests somewhat accurately" $ \(port, _, overhead, predictedDist, ref) -> do
         threadDelay 500000
         let key = "key"
         allStats <- urlStatsToDist . aPerUrl <$> runWrecker port
         putStrLn $ "allStats" ++ show allStats
         
         putStrLn $ "overhead" ++ show overhead
         
         putStrLn $ "predictedDist" ++ show predictedDist
         
         expectedDist <- urlStatsToDist . aPerUrl <$> NextRef.readLast ref

         putStrLn $ "expectedDist" ++ show expectedDist

         
         let adjustedDist = substractDist allStats overhead
         putStrLn $ "adjustedDist" ++ show adjustedDist
         putStrLn "\n\n"
         putStrLn $ "error" ++ show (substractDist expectedDist adjustedDist)
         adjustedDist `shouldBeApprox` expectedDist
