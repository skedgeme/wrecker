{-# LANGUAGE ScopedTypeVariables, TypeOperators, OverloadedStrings, DeriveGeneric, FlexibleInstances, QuasiQuotes, DeriveAnyClass, CPP, FlexibleContexts, UndecidableInstances #-}
import Web.Scotty (ScottyM, ActionM, json)
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
import Data.Aeson hiding (json)
import Data.Text (Text)
import Data.Reflection
import qualified Web.Scotty as Scotty
import Data.Proxy
import Control.Lens.Lens (ALens')
import Data.Constraint
import System.Random.MWC.Distributions
import System.Random.MWC
import qualified Data.Text as T
import Data.Monoid

newtype Envelope a = Envelope { value :: a }
  deriving (Show, Eq, Generic, ToJSON)

rootRef :: Int -> Text
rootRef port = T.pack $ "http://localhost:" ++ show port

jsonE :: ToJSON a => a -> ActionM ()
jsonE = json . Envelope

app :: Port -> ScottyM ()
app port = do
  let host = rootRef port
  Scotty.get "/root" $ jsonE [aesonQQ|
     { "products" : #{host <> "/products" }
     , "carts"    : #{host <> "/carts"    }
     , "users"    : #{host <> "/users"    }
     , "login"    : #{host <> "/login"    }
     , "checkout" : #{host <> "/checkout" }
     }
   |]

  Scotty.get "/products" $ jsonE [aesonQQ|
     [ #{host <> "/products/0"}
     ]
   |]
 
  Scotty.get "/product/:id" $ jsonE [aesonQQ|
     { "summary" : "shirt" }
   |]

  Scotty.get "/carts" $ jsonE [aesonQQ|
     [ #{host <> "/carts/0"}
     ]
   |]

  Scotty.get "/carts/:id" $ jsonE [aesonQQ|
     { "items" : #{host <> "/carts/0/items"}
     }
   |]
 
  Scotty.post "/carts/:id/items" $ jsonE [aesonQQ|
      #{host <> "/carts/0/items"}
   |]

  Scotty.get "/users" $ jsonE [aesonQQ|
     [ #{host <> "/users/0"}
     ]
   |]
 
  Scotty.get "/users/:id" $ jsonE [aesonQQ|
     { "cart"     : #{host <> "/carts/0"}
     , "username" : "example"
     }
   |]

  Scotty.post "/login"    $ jsonE [aesonQQ|
      #{host <> "/users/0"}
    |]
  Scotty.post "/checkout" $ jsonE ()

run :: IO (Port, ThreadId)
run = do 
  (port, socket) <- openFreePort 

  let settings = setPort port $ defaultSettings 
      options  = def { Scotty.settings = settings }
                  
  gen <- liftIO createSystemRandom
  threadId <- forkIO 
             $ Scotty.scottySocket options socket 
             $ app port
                
  
  return (port, threadId)
  
stop :: (Port, ThreadId) -> IO ()
stop (_, threadId) = killThread threadId

main :: IO ()
main = Scotty.scotty 3000 $ app 3000
