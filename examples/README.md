

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
import qualified Client as Client
import qualified Server as Server
import Wrecker (run, runParser)
import Data.Function (fix)
import Control.Exception (handle, IOException)
import Control.Concurrent ( threadDelay
                          , forkIO
                          , newEmptyMVar
                          , takeMVar
                          , putMVar
                          )
import Network.Connection ( connectTo
                          , connectionClose
                          , ConnectionParams (..)
                          , initConnectionContext
                          )
import Control.Monad (void)
```

A little utility function which loops until a port is ready for connections.

```haskell
waitFor :: Int -> IO ()
waitFor port = do
    cxt <- initConnectionContext
    fix $ \next -> do
        handle (\(e :: IOException) -> threadDelay 100000 >> next)
               (do
                  connection <- connectTo cxt
                              $ ConnectionParams "localhost"
                                                 (fromIntegral port)
                                                 Nothing
                                                 Nothing
                  connectionClose connection
               )
```

```haskell
main :: IO ()
main = do
 -- Start the server on it's own thread
 forkIO $ Server.main

 -- The examples use port 3000 by default
 let port = 3000


 options <- runParser
 -- wait for the server to be ready
 waitFor port

 -- Start the client and close an MVar to signal when the thread has finished
 end <- newEmptyMVar
 result <- forkIO $ do
     run options $ Client.benchmarks port
     putMVar end ()

 -- Wait for the client thread to finish and then return
 takeMVar end
```
