### `wrecker`
`wrecker` is a HTTP benchmarking library for profiling several API actions.

`wrecker`'s API types are type alias, so clients can conform to the interface without depending on the `wrecker` package.

### Example

Here is an example client script.

```
testScript :: Int -> Recorder -> IO ()
testScript port recorder = do
  Root { products
       , login
       , checkout
       }             <- get recorder "root"     (rootRef port)
  firstProduct : _   <- get recorder "products" products
  userRef            <- rpc recorder "login"    login
                                                ( Credentials
                                                  { userName = "a@example.com"
                                                  , password = "password"
                                                  }
                                                )
  User { usersCart } <- get recorder "user"     userRef
  Cart { items }     <- get recorder "cart"     usersCart

  insert "items" items firstProduct
  rpc "checkout" checkout cart
```

For this example `stub`<sup>[1](#footnote1)</sup> <sup>[2](#footnote2)</sup> server.

```json
{ "products"        : ["http://localhost:3000/products/0"]
, "product/:id"     : { "summary" : "shirt" }
, "carts"           : ["http://localhost:3000/carts/0"]
, "carts/:id"       : { "items" : "http://localhost:3000/carts/0/items" }
, "carts/:id/items" : []
, "users"           : ["http://localhost:3000/users/0"]
, "users/:id"       : { "cart"     : "http://localhost:3000/carts/0"
                    , "username" : "example"
                    }
}
```

### Output

```bash
cabal run example -- --concurrency=20 --run-count=4 --display-mode=Interactive
```

![Example terminal output](/examples/example.gif?raw=true "Example Terminal Output")


### Running Examples
 - To run whole benchmark example `cabal run example`
 - Just the client `cabal run example-client `
 - Just the server `cabal run example-server`

# How to Use `wrecker` to Write Your Own Benchmarks

[The documentation for Client.lhs is also a tutorial. Click here.] (/examples/Client.md)

# examples/Main.lhs Implementation
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
        handle (\(e :: IOException) -> threadDelay 100000 >> next) $ do
            connection <- connectTo cxt
                        $ ConnectionParams "localhost"
                                           port
                                           Nothing
                                           Nothing
            connectionClose connection
```
We start the [server](/examples/Server.hs) and then when it is ready we start the `wrecker` [client](/examples/Client.md)

```
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

<a name="footnote1">1</a>: See Martin Fowler Stackoverflow *highest ranked answer* [http://stackoverflow.com/questions/346372/whats-the-difference-between-faking-mocking-and-stubbing]

<a name="footnote1">2</a>: Converting to a `fake` is left as an exercise.
