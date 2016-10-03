### `wrecker`
`wrecker` is an HTTP benchmarking library for profiling several API actions.

### Output

Output from running

```bash
cabal run example -- --concurrency=1000 --run-timed=1000000000 --interactive +RTS -N -I0
```

![Example terminal output](/examples/new-example.gif?raw=true "Example Terminal Output")

### Example Script

```haskell
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

### Memory Use

`wrecker` calculates statistics incrementally and is able to use a constant amount of memory regardless of the length of time it is run.

Here is a heap snapshot for a thousand concurrent connections

![Heap Snapshot](/memoryProfile.png?raw=true "Heap Snapshot")

### Running Examples
 - Run the client and server example with `cabal run example`
 - Run the client with `cabal run example-client `
 - Run the serve with `cabal run example-server`

# How to Create Benchmarks with `wrecker`

[Typed REST client tutorial](/examples/Client.md)
