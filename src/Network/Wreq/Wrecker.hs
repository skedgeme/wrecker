module Network.Wreq.Wrecker where
import Wrecker
import qualified Network.Wreq.Session as Session
import Network.Connection (ConnectionContext)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Data.ByteString.Lazy as L
import qualified Network.Wreq.Types as Wreq
import Data.Default (def)

data Session = Session
  { sSession  :: Session.Session
  , sRecorder :: Recorder
  }



-- |
-- Module      : Network.Wreq.Internal.Types
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- HTTP client types.

-- All of this code below was copied from bos's `Network.Wreq.Session`

defaultManagerSettings :: ConnectionContext -> HTTP.ManagerSettings
defaultManagerSettings context = (TLS.mkManagerSettingsContext (Just context) def Nothing)
                  { HTTP.managerResponseTimeout = HTTP.responseTimeoutNone }

-- | Create a 'Session', passing it to the given function.  The
-- 'Session' will no longer be valid after that function returns.
--
-- This session manages cookies and uses default session manager
-- configuration.
withSession :: ConnectionContext -> Recorder -> (Session -> IO a) -> IO a
withSession context recorder
  = withSessionControl recorder
                       (Just (HTTP.createCookieJar []))
                       (defaultManagerSettings context)

-- | Create a session.
--
-- This uses the default session manager settings, but does not manage
-- cookies.  It is intended for use with REST-like HTTP-based APIs,
-- which typically do not use cookies.
withAPISession :: ConnectionContext -> Recorder -> (Session -> IO a) -> IO a
withAPISession context recorder
  = withSessionControl recorder
                       Nothing
                       (defaultManagerSettings context)

-- | Create a session, using the given cookie jar and manager settings.
withSessionControl :: Recorder
                   -> Maybe HTTP.CookieJar
                   -- ^ If 'Nothing' is specified, no cookie management
                   -- will be performed.
                   -> HTTP.ManagerSettings
                   -> (Session -> IO a) -> IO a
withSessionControl recorder cookie settings f
  = Session.withSessionControl cookie settings
  $ \session -> f (Session session recorder)

-- this records things. It's not ideal, but an more acurate
-- implementation is harder. Pull requests welcome.
withSess :: (Session.Session -> String -> IO a) -> Session -> String -> IO a
withSess f sess key = record (sRecorder sess) key $  f (sSession sess) key

withSess1 :: (Session.Session -> String -> a -> IO b) -> Session -> String -> a -> IO b
withSess1 f sess key b = record (sRecorder sess) key $  f (sSession sess) key b

-- | 'Session'-specific version of 'Network.Wreq.get'.
get :: Session -> String -> IO (HTTP.Response L.ByteString)
get = withSess Session.get

-- | 'Session'-specific version of 'Network.Wreq.post'.
post :: Wreq.Postable a
     => Session
     -> String
     -> a
     -> IO (HTTP.Response L.ByteString)
post = withSess1 Session.post

-- | 'Session'-specific version of 'Network.Wreq.head_'.
head_ :: Session -> String -> IO (HTTP.Response ())
head_ = withSess Session.head_

-- | 'Session'-specific version of 'Network.Wreq.options'.
options :: Session -> String -> IO (HTTP.Response ())
options = withSess Session.options

-- | 'Session'-specific version of 'Network.Wreq.put'.
put :: Wreq.Putable a => Session -> String -> a -> IO (HTTP.Response L.ByteString)
put = withSess1 Session.put

-- | 'Session'-specific version of 'Network.Wreq.delete'.
delete :: Session -> String -> IO (HTTP.Response L.ByteString)
delete = withSess Session.delete

-- | 'Session'-specific version of 'Network.Wreq.getWith'.
getWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response L.ByteString)
getWith opts = withSess (Session.getWith opts)

-- | 'Session'-specific version of 'Network.Wreq.postWith'.
postWith :: Wreq.Postable a => Wreq.Options -> Session -> String -> a
         -> IO (HTTP.Response L.ByteString)
postWith opts = withSess1 (Session.postWith opts)

-- | 'Session'-specific version of 'Network.Wreq.headWith'.
headWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response ())
headWith opts = withSess (Session.headWith opts)

-- | 'Session'-specific version of 'Network.Wreq.optionsWith'.
optionsWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response ())
optionsWith opts = withSess (Session.optionsWith opts)

-- | 'Session'-specific version of 'Network.Wreq.putWith'.
putWith :: Wreq.Putable a => Wreq.Options -> Session -> String -> a
        -> IO (HTTP.Response L.ByteString)
putWith opts = withSess1 (Session.putWith opts)

-- | 'Session'-specific version of 'Network.Wreq.deleteWith'.
deleteWith :: Wreq.Options -> Session -> String -> IO (HTTP.Response L.ByteString)
deleteWith opts = withSess (Session.deleteWith opts)
