{-# LANGUAGE QuasiQuotes #-}
module WreckerSpec where
import Wrecker
import Test.Hspec 
import WreckerTest.Server (run)
import Network.Wreq
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

-- Some helper types useful for our journey
-- The Ref is implement as a string, which should
-- contain a URL
data Ref a   = Ref { unRef :: Text } 
  deriving (Show, Eq)
  
instance FromJSON (Ref a) where
  parseJSON = withText "FromJSON (Ref a)" Ref
-- RPC calls are also represented as URLS
data RPC a b = RPC String
  deriving (Show, Eq)
  
instance FromJSON (RPC a b) where
  parseJSON = withText "FromJSON (Ref a)" RPC

-- Generic API functions
-- we wraps Wreq HTTP calls with 'record'

get :: FromJSON a => Recorder -> String -> Ref a -> IO a
get recorder key (Ref url) 
    = fmap unEnvelope 
  =<< asJSON 
  =<< record recorder key (Wreq.get url)

add :: (ToJSON a, FromJSON a) => Recorder -> String -> Ref [a] -> a -> IO ()
add recorder key (Ref url) x 
  = void $ record recorder key $ Wreq.post url $ toJSON x

rpc :: (ToJSON a, FromJSON b) => Recorder -> String -> RPC a b -> a -> IO b
rpc recorder key (RPC url) x 
    = fmap unEnvelope 
  =<< asJSON 
  =<< record recorder key (Wreq.post url $ toJSON x)

{-
  First we will make a client for REST API 

  For this example imagine a simple ecommerce API

-}
data Root = Root
  { products :: Ref [Ref Product]
  , carts    :: Ref [Ref Cart   ]
  , users    :: Ref [Ref User   ]
  , login    :: RPC Credentials (Ref User)
  , checkout :: RPC (Ref Cart)  ()
  } deriving BoilerPlate

data Product = Product
  { summary :: Text
  } 

data Cart = Cart 
  { items :: Ref [Ref Product]
  }

data User = User
  { cart        :: Ref Cart
  , credentials :: Credentials
  }

-- Main entry point
rootRef :: Port -> Ref Root
rootRef port = Ref $ "http://localhost:" ++ show port ++ "/root"

-- The test script
testScript :: Port -> Recorder -> IO ()
testScript port recorder = do 
  let get' = get recorder
      add' = add recorder
      rpc' = rpc recorder

  -- Get the root relations and unpack the resource refs for further calls
  Root {login, products} <- get' "root"     (rootRef port)
  userRef                <- rpc' "login"    login 
                                             ( Credentials 
                                               { userName = "a@example.com"
                                               , password = "password"
                                               }
                                             )
  User {cart }           <- get' "user"     userRef
  Cart {items}           <- get' "cart"     cart
  -- We get all products and name the first one
  firstProduct : _       <- get' "products" products
  _                      <- add' "items"    items    firstProduct
  _                      <- rpc' "checkout" checkout cart

  return ()
  
runTestScript :: IO AllStats
runTestScript = run defaultOptions [("test", testScript port)]

-- Create a distribution for sleeping
distribution :: RootDistribution
distribution = RootDistribution 
  { productsDistribution = normal
  , cartsDistribution    = normal
  , usersDistribution    = normal
  , rootDistribution     = normal
  }

main :: IO ()
main = hspec spec

resultStatsToDistribution :: ResultStats -> Distribution
resultStatsToDistribution = undefined

-- There is a better way
approxDouble :: Double -> Double -> Bool
approxDouble x y = abs (x - y) < 0.001

approxEq :: Distribution -> Distribution -> Bool
approxEq x y = case (x, y) of
  (Gaussian a b, Gaussian a' b') -> approxDouble a a'
                                 && approxDouble b b'

spec :: Spec 
spec = beforeAll (run distribution) $ afterAll stop $ describe "Wrecker" $ 
  it "measure requests somewhat accurately" $ \(port, _) -> 
    allStats <- runTestScript
    let perUrlStats = aPerUrl allStats
        Just rootStats     = H.lookup rootKey  perUrlStats
        resultStatsToDistribution rootStats `shouldBeEssentially` rootDistribution
        
        Just loginStats    = H.lookup loginKey perUrlStats
        resultStatsToDistribution rootStats `shouldBeEssentially` loginDistribution
        
        Just productsStats = H.lookup productsKey perUrlStats
        resultStatsToDistribution rootStats `shouldBeEssentially` productsDistribution
        
        Just addItemStats  = H.lookup itemsKey perUrlStats
        resultStatsToDistribution addItemStats `shouldBeEssentially` addItemDistribution
        
        Just checkoutStats = H.lookup checkoutKey perUrlStats
        resultStatsToDistribution checkoutStats `shouldBeEssentially` checkoutDistribution