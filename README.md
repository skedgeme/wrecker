### `wrecker`
`wrecker` is a HTTP benchmarking library for profiling several API actions.

### Output

Output from running

```bash
cabal run example -- --concurrency=1000 --run-timed=1000000000 --interactive +RTS -N -I0
```

![Example terminal output](/examples/new-example.gif?raw=true "Example Terminal Output")

### Example Script

```
testScript :: Int -> ConnectionContext -> Recorder -> IO ()
testScript port cxt rec = withSession cxt rec $ \sess -> do
  Root { products
       , login
       , checkout
       }             <- get sess (rootRef port)
  firstProduct : _   <- get sess products
  userRef            <- rpc sess login
                                  ( Credentials
                                    { userName = "a@example.com"
                                    , password = "password"
                                    }
                                  )
  User { usersCart } <- get sess userRef
  Cart { items }     <- get sess usersCart

  insert sess items firstProduct
  rpc sess checkout cart
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
