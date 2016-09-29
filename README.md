### `wrecker`
`wrecker` is a HTTP benchmarking library for profiling several API actions.

### Output

Output from running

```bash
cabal run example -- --concurrency=20 --run-count=4 --display-mode=Interactive
```

![Example terminal output](/examples/example.gif?raw=true "Example Terminal Output")

### Example Script

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

For this example `stub` server.

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

### Memory Use 

`wrecker` calculates statistics incrementally and is able to use a constant amount of memory regardless of the length of time it is run.

Here is a heap snapshot for a thousand concurrent connections.

![Heap Snapshot](/memoryProfile.png?raw=true "Heap Snapshot")

### Running Examples
 - To run whole benchmark example `cabal run example`
 - Just the client `cabal run example-client `
 - Just the server `cabal run example-server`

# How to Use `wrecker` for Our Own Benchmarks

[Example client tutorial](/examples/Client.md)
