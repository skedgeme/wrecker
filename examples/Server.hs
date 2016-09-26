{-# LANGUAGE ScopedTypeVariables
           , TypeOperators
           , OverloadedStrings
           , DeriveGeneric
           , FlexibleInstances
           , QuasiQuotes
           , DeriveAnyClass
           , CPP
           , FlexibleContexts
           , UndecidableInstances
           , RecordWildCards
           , DeriveFunctor
           , LambdaCase
           , RecursiveDo
           , OverloadedStrings
#-}

#ifndef _SERVER_IS_MAIN_
module Server where
#endif

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
import Distribution
import Wrecker
import Control.Exception
import Control.Concurrent.NextRef (NextRef)
import qualified Control.Concurrent.NextRef as NextRef
import qualified Control.Immortal as Immortal
import qualified Wrecker.Runner as Wrecker
import qualified Wrecker.Statistics as Wrecker
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import Network.Socket

newtype Envelope a = Envelope { value :: a }
  deriving (Show, Eq, Generic, ToJSON)

rootRef :: Int -> Text
rootRef port = T.pack $ "http://localhost:" ++ show port

jsonE :: ToJSON a => a -> ActionM ()
jsonE = json . Envelope

sleepDist :: GenIO -> Distribution -> ActionM ()
sleepDist gen dist = liftIO $ threadDelay . floor . (*1e6) =<< sample gen dist

data Root a = Root
  { root            :: a
  , products        :: a
  , cartsIndex      :: a
  , cartsIndexItems :: a
  , usersIndex      :: a
  , login           :: a
  , checkout        :: a
  } deriving (Show, Eq, Functor)

type RootDistribution = Root Distribution

instance Applicative Root where
 pure x = Root
          { root            = x
          , products        = x
          , login           = x
          , usersIndex      = x
          , cartsIndex      = x
          , cartsIndexItems = x
          , checkout        = x
          }
 
 f <*> x = Root
           { root            = root            f $ root                 x
           , products        = products        f $ products             x
           , login           = login           f $ login                x
           , usersIndex      = usersIndex      f $ usersIndex           x
           , cartsIndex      = cartsIndex      f $ cartsIndex           x
           , cartsIndexItems = cartsIndexItems f $ cartsIndexItems      x
           , checkout        = checkout        f $ checkout             x

           }  

zeroDistribution :: RootDistribution
zeroDistribution = pure $ Constant 0

app :: GenIO -> RootDistribution -> Port -> ScottyM ()
app gen Root {..} port = do
  let host = rootRef port
  Scotty.get "/root" $ do
    sleepDist gen root
    jsonE [aesonQQ|
           { "products" : #{host <> "/products" }
           , "carts"    : #{host <> "/carts"    }
           , "users"    : #{host <> "/users"    }
           , "login"    : #{host <> "/login"    }
           , "checkout" : #{host <> "/checkout" }
           }
          |]

  Scotty.get "/products" $ do
    sleepDist gen products
    jsonE [aesonQQ|
             [ #{host <> "/products/0"}
             ]
           |]

  Scotty.get "/product/:id" $ do
    sleepDist gen products
    jsonE [aesonQQ|
          { "summary" : "shirt" }
          |]

  Scotty.get "/carts" $ do
    -- sleepDist gen carts
    jsonE [aesonQQ|
          [ #{host <> "/carts/0"}
          ]
          |]

  Scotty.get "/carts/:id" $ do
    sleepDist gen cartsIndex
    jsonE [aesonQQ|
          { "items" : #{host <> "/carts/0/items"}
          }
          |]

  Scotty.post "/carts/:id/items" $ do
    sleepDist gen cartsIndexItems
    jsonE [aesonQQ|
          #{host <> "/carts/0/items"}
          |]

  Scotty.get "/users" $ do
    --sleepDist gen users
    jsonE [aesonQQ|
          [ #{host <> "/users/0"}
          ]
          |]

  Scotty.get "/users/:id" $ do
    sleepDist gen usersIndex
    jsonE [aesonQQ|
          { "cart"     : #{host <> "/carts/0"}
          , "username" : "example"
          }
          |]

  Scotty.post "/login" $ do
    sleepDist gen login
    jsonE [aesonQQ|
          #{host <> "/users/0"}
          |]

  Scotty.post "/checkout" $ do
    sleepDist gen checkout
    jsonE ()

run :: RootDistribution -> IO (Port, Immortal.Thread, ThreadId, NextRef AllStats)
run = start Nothing

stop :: (Port, ThreadId, NextRef AllStats) -> IO AllStats
stop (_, threadId, ref) = do
  killThread threadId
  NextRef.readLast ref

toKey :: Wai.Request -> String
toKey x = case Wai.pathInfo x of
  ["root"]                  -> "root"
  ["products"]              -> "products"
  "carts" : _ : "items" : _ -> "items"
  "carts" : _ : _           -> "cart"
  "users" : _               -> "user"
  ["login"]                 -> "login"
  ["checkout"]              -> "checkout"

recordMiddleware :: Recorder -> Wai.Application -> Wai.Application
recordMiddleware recorder app req sendResponse 
  = record recorder (toKey req) $! app req $ \res -> sendResponse res
  
getASocket :: Maybe Port -> IO (Port, Socket)
getASocket = \case
  Just port -> undefined
  Nothing   -> openFreePort
  
start :: Maybe Port -> RootDistribution -> IO ( Port
                                              , Immortal.Thread
                                              , ThreadId
                                              , NextRef AllStats
                                              )
start mport dist = do
  (port, socket) <- getASocket mport
  gen <- createSystemRandom
  
  (ref, recorderThread, recorder) <- newStandaloneRecorder
  scottyApp <- Scotty.scottyApp $ app gen dist port
  gen       <- createSystemRandom
  threadId <- forkIO $ Warp.runSettingsSocket defaultSettings socket 
                     $ recordMiddleware recorder
                     $ scottyApp

  return (port, recorderThread, threadId, ref)

main :: IO ()
main = mdo 
  (_, recorderThread, _,ref) <- start (Just 3000) zeroDistribution 
                  `onException` ( do Immortal.stop recorderThread
                                     allStats <- NextRef.readLast ref
                                     putStrLn $ Wrecker.pprStats Nothing allStats
                                )
  return ()

  