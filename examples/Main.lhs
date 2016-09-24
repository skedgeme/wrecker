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

For this example `stub`[1][2] server.

```json
 { "products"         : ["http://localhost:3000/products/0"]
 , "product/:id"      : { "summary" : "shirt" }
 , "carts"            : ["http://localhost:3000/carts/0"]
 , "carts/:id"        : { "items" : "http://localhost:3000/carts/0/items" }
 , "carts/:id/items" : []
 , "users"           : ["http://localhost:3000/users/0"]
 , "users/:id"       : { "cart"     : "http://localhost:3000/carts/0" 
                       , "username" : "example"
                       }
}
```

### Output

Output from running 

```bash
cabal run example -- --concurrency=20 --run-count=4 --display-mode=Interactive
```

![Example terminal output](/example.gif?raw=true "Example Terminal Output")


### Running Examples
 - To run whole benchmark example `cabal run example` 
   - `cabal run example -- --help` for help
   - `cabal run example -- --concurrency=30 --run-count=5 --display-mode=Interactive` for something more interesting.
 - Just the client `cabal run example-client `
   - `cabal run example-client -- --help` for help
 - Just the server `cabal run example-client` 
   - ...

# How To Use

See this literate haskell file here [https://github.com/skedgeme/wrecker/blob/example-progress/examples/Client.md] 

[1] See Martin Fowler Stackoverflow *highest ranked answer* [http://stackoverflow.com/questions/346372/whats-the-difference-between-faking-mocking-and-stubbing] 

[2] Converting to a `fake` is left as an exercise.

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

waitFor3000 :: IO ()
waitFor3000 = do 
    cxt <- initConnectionContext
    fix $ \next -> do 
        handle (\(e :: IOException) -> threadDelay 100000 >> next) $ do
            connection <- connectTo cxt $ ConnectionParams "localhost" 
                                                       3000 
                                                       Nothing 
                                                       Nothing
            connectionClose connection 
    

main :: IO ()
main = do 
 forkIO $ Server.main
 
 waitFor3000 
 options <- runParser
 end <- newEmptyMVar
 result <- forkIO $ do 
     run options $ Client.benchmarks 3000
     putMVar end ()
 
 takeMVar end
```

