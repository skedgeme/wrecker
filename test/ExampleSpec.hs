{-# LANGUAGE QuasiQuotes, LambdaCase, RecordWildCards #-}
module ExampleSpec where
import Wrecker
import qualified Wrecker.Statistics as Stats
import Test.Hspec
import qualified Server as Server
import qualified Client as Client
import Network.Wreq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Concurrent
import Network.Wai.Handler.Warp (Port)
import Data.Maybe (fromJust)
import qualified Statistics.Sample as S
import Control.Monad (replicateM)
import System.Clock.TimeIt
import qualified Data.Vector.Unboxed as U
import Control.Concurrent.NextRef (NextRef)
import qualified Control.Concurrent.NextRef as NextRef

{-
  This file tests how well `wrecker` can detect a "signal". 
  Essentially we increase the time of server and see how close `wrecker`
  gets to detecting the increase. 

  The precision of `threadDelay` is not great at least for small values. So we 
  record the actual times on the server and compare against the measured values.

  The model we use

       client observed time(delay) = travel time + server overhead + request time(delay)
  
  We call `travel time + server overhead` `overhead` and compute it by measuring

       overhead = client observed time(1 microsecond) - request time(1 microsecond)

  We assume that `overhead` is the same regardless of the delays injected to 
  slow down `request time`. (this appears false unfortunately and affects the accuracy).

  Then we compare

      client observed time(10 milliseconds) - overhead ~ recorded from the server(10 millisecond delay)

  Telling the server to sleep 10 milliseconds actually slows it down around 12. 

  `wrecker` is consistantly overestimating the times. I don't know if this an issue with wrecker or 
  something about the interplay between delaying the request and the travel time or the server overhead.


  There are many issues with this "test". The interations should be based on some statistically stopping conditioning. 
  Additionally the approx equality should really be something like a .

  The gc is an issue for getting good data. Running with -I0 helps, -qg might?
-}

sleepAmount :: Int  -- microseconds
sleepAmount = 10000 -- 10 milliseconds

              
data Gaussian = Gaussian 
                { mean     :: Double
                , variance :: Double
                } deriving (Show, Eq)
                
subtractGaussian :: Gaussian -> Gaussian -> Gaussian
subtractGaussian x y 
  = Gaussian (mean     x - mean     y) 
             (variance x - variance y)

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
rootDistribution :: Server.RootInt
rootDistribution = pure sleepAmount

main :: IO ()
main = hspec spec

toDist :: ResultStatistics -> Gaussian
toDist x 
  = let stats = rs2xx x 
  in Gaussian (Stats.mean stats) (Stats.variance stats)


class Approx a where
  approx :: a -> a -> Bool

instance Approx Double where
  approx x y = abs (x - y) < 0.002

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
                    { runStyle    = RunCount 100
                    , displayMode = Interactive
                    }
                  )
                  [ (key, Client.testScript port)
                  ]

calculateOverhead :: IO (Server.Root Gaussian)
calculateOverhead = do 
  (port, _, threadId, ref) <- Server.run $ pure 1
  
  allStats    <- runWrecker port
  
  -- This how long a 'null' request takes
  serverStats <- NextRef.readLast ref
  putStrLn $ Stats.pprStats Nothing serverStats
  killThread threadId
  
  return $ substractDist (urlStatsToDist $ aPerUrl allStats   ) 
                         (urlStatsToDist $ aPerUrl serverStats)
         
  
start :: IO (Port, ThreadId, Server.Root Gaussian, NextRef AllStats)
start = do
  overhead <- calculateOverhead
  (port, _, threadId, ref) <- Server.run rootDistribution
    
  return (port, threadId, overhead, ref)
  
stop :: (Port, ThreadId, Server.Root Gaussian, NextRef AllStats) -> IO ()
stop (_, threadId, _, _) = killThread threadId

shouldBeApprox :: (Show a, Approx a) => a -> a -> IO ()
shouldBeApprox x y = shouldSatisfy (x, y) (uncurry approx)

spec :: Spec
spec = beforeAll start
     $ afterAll  stop
     $ describe "Wrecker"
     $ it "measure requests somewhat accurately" $ \(port, _, overhead, ref) -> do
         let key = "key"
         allStats <- urlStatsToDist . aPerUrl <$> runWrecker port

         expectedDist <- urlStatsToDist . aPerUrl <$> NextRef.readLast ref
         
         let adjustedDist = substractDist allStats overhead

         adjustedDist `shouldBeApprox` expectedDist
