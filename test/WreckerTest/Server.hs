{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveAny, CPP #-}
module WreckerTest.Server where
import Web.Scotty
import Control.Concurrent
import Data.Aeson.QQ
import Control.Monad.IO.Class
import Network.Wai.Handler.Warp 
  ( defaultSettings
  , setPort
  , openFreePort
  , Port (..)
  )
import Data.Default
import GHC.Generics

#define BoilerPlate (Show, Eq, ToJSON, FromJSON, Generic)

data Piece = I Int | S String

class ToPath (RPC a b) where
  toPath (RPC key f) = [S key]

class Resource a where
  resource :: p a -> Piece

newtype Path = Path [Piece]

instance Give Text => ToJSON Path where
  toJSON (Path path)= pprPath $ S give : path

class ToPath a where
  toPath :: a -> [Piece]

instance  ToPath (RPC a b) where
  toPath (RPC key f) = [S f]

instance Resource a => ToPath (NodeGroup a) where
  toPath (NodeGroup key ns) = [resource (Proxy :: Proxy a)]
  
instance  Resource a => ToPath (Node a) where
  toPath (Node index n) = [resource (Proxy :: Proxy a), I index]
  
instance  Resource a => ToPath (Edge a b) where
  toPath (Edge index key l) = [ resource (Proxy :: Proxy a)
                              , I index
                              , S key 
                              ]

newtype RPC       a b = RPC       Text     (a -> b)
newtype NodeGroup a   = NodeGroup          [Node a]
data    Node      a   = Node      Int            a
data    Edge      a b = Edge      Int Text (Lens' a b)

newtype Envelope a = Envelope { unEnvelope :: a }



data Product = Product
  { summary :: Text
  }

data Cart = Cart 
  { items :: [Node Product]
  }

data User = User
  { cart        :: Node Cart
  , credentials :: Credentials
  }

data Credentials = Credentials 
  { userName  :: String
  , password  :: String
  } deriving BoilerPlate

checkout :: RPC (Ref Cart) ()
checkout = undefined

data Root = Root
  { products :: NodeGroup Product
  , carts    :: NodeGroup Cart
  , users    :: NodeGroup User
  , login    :: RPC Credentials (Node User)
  , checkout :: RPC (Ref Cart)  ()
  }   

indices :: [Int]
indices = [0..]

---
--- Test Data
--- 
products :: NodeGroup Products
products = NodeGroup $ zipWith Node indices 
  [ Product "shirt"
  , Product "hat"
  ] 
  
carts :: NodeGroup Cart
carts = NodeGroup $ zipWith Node indices
  [ Cart []
  ]
  
users :: NodeGroup User
users = NodeGroup $
  [ User (Node 0 carts) "user0" "password"
  ]
  
login :: RPC Credentials (Node User)
login = PRC $ \_ -> Node 0 users
  
root :: Root
root = Root {..}

--
-- Distribution
--

data RootDistribution = RootDistribution
  { productsDistribution :: Distribution
  , cartsDistribution    :: Distribution
  , addItemDistribution  :: Distribution
  , usersDistribution    :: Distribution
  , rootDistribution     :: Distribution
  , checkoutDistribution :: Distribution
  }
  
class HasRootDistribution g where
  asRootDist :: g -> RootDistribution
  
instance HasRootDistribution (RootDistribution, a) where
  asRootDist = fst
  
class GetDistribution a where
  getDist :: HasRootDistribution g => p a -> g -> Distribution
  
instance GetDistribution Root where
  getDist _ = rootDistribution . asRootDist

sleep :: (HasRootDistribution g, Give g, GetDistribution x) => Proxy x -> Scotty 
sleep p = liftIO $ threadDelay $ getDist p give

get :: ( ToJSON x
       , Give g
       , HasRootDistribute g
       , HasHost g
       , GetDistribute x
       ) 
    => String
    -> x 
    -> Scotty
get route x = 
  Scotty.get route $ do
    sleep (Proxy :: Proxy x)
    json $ Envelope x

rest :: Port -> SleepDistribution -> Scotty
rest port x = give (x, host port) $ do 
  get "/root"           root
  rpc "/login"          login
  get "/products"       products
  add "/cart/:id/items" items
  rpc "/checkout"       checkout

run :: IO (Port, ThreadId)
run = do 
  (port, socket) <- openFreePort 
  let settings = setPort port $ defaultSettings 
      options  = def { settings     = settings }
  threadId <- forkIO $ scottySocket options socket $ do
                rest defaultSleepDistrubtion
  
  return (port, threadId)
  
stop :: (Port, ThreadId) -> IO ()
stop (_, threadId) = killThread threadId