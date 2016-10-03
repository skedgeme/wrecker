## Running the Examples

- Run the whole benchmark example with `cabal run example`
- Run just the client with `cabal run example-client `
- Run just the server with `cabal run example-server`

Additionally, the examples take the standard `wrecker` command line arguments, which can be viewed with

     cabal run example -- --help

```
wrecker - HTTP stress tester and benchmarker

Usage: example [--concurrency ARG] [--bin-count ARG] ([--run-count ARG] |
               [--run-timed ARG]) [--timeout-time ARG] [--display-mode ARG]
               [--log-level ARG] [--match ARG] [--request-name-size ARG]
               [--output-path ARG] [--silent]
  Welcome to wrecker

Available options:
  -h,--help                Show this help text
  --concurrency ARG        Number of threads for concurrent requests
  --bin-count ARG          Number of bins for latency histogram
  --run-count ARG          number of times to repeat
  --run-timed ARG          number of seconds to repeat
  --timeout-time ARG       How long to wait for all requests to finish
  --display-mode ARG       Display results interactively
  --log-level ARG          Log to stderr events of criticality greater than the LOG_LEVEL
  --match ARG              Only run tests that match the glob
  --request-name-size ARG  Request name size for the terminal display
  --output-path ARG        Save a JSON file of the the statistics to given path
  --silent                 Disable all output
```

Below is the source for `example`, which creates a client and server:

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
```

A little utility function which loops until a port is ready for connections:

```haskell
waitFor :: Int -> IO ()
waitFor port = do
    cxt <- initConnectionContext
    fix $ \next -> do
        handle (\(_ :: IOException) -> threadDelay 100000 >> next)
               (do
                  connection <- connectTo cxt
                              $ ConnectionParams "localhost"
                                                 (fromIntegral port)
                                                 Nothing
                                                 Nothing
                  connectionClose connection
               )
```

Entry point:

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
 forkIO $ do
     run options =<< Client.benchmarks port
     putMVar end ()

 -- Wait for the client thread to finish and then return
 takeMVar end
```
