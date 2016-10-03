### `wrecker`
`wrecker` is an HTTP benchmarking library and executable for profiling several API actions.

### Why?

There are plenty of HTTP profilers in existence, `wrk`, `ab`, `JMeter`, etc. Most profilers provide some facility for scripting and using the responses of the previous requests. However, the scripting facility is usually weak and difficult to use.

`wreck` is designed from the ground up for scripting complex API interactions. Benchmarks can utilize a `wreq` interface, along with the full benefits of using Haskell.

### Quick Start

`wrecker` provides a simple executable [`wreck`](/app/Main.hs) which takes single URL to profile.

```
$ wreck http://localhost:3000/root
```

The same functionality can executed from ghci, listed below is a similar one liner.

```bash
$ ghci
> import Wrecker
> import Network.Wreq.Wrecker
> runOne defaultOptions $ \env -> withWreq env $ get sess "http://localhost:3000/root"
```

Running with ghci is okay to get a feel for `wrecker` but it is recommended that all benchmarks are compiled with optimizations, the threaded library,
and run with the RTS options `-N -I0 -qg`.

In addition to benchmarking a single URL, the `wrecker` library can create
multiple named benchmarks, each of which can contain multiple endpoints to
profile.

The example below uses the `wrecker`'s `wreq` interface but any `http-client` based library could be used.

```haskell
import Wrecker
import Network.Wreq.Wrecker

main = defaultMain
  [ ( "first page"
    , \env -> withWreq env $ \sess -> do
        get sess "http://localhost:3000/page1/page1.css"
        get sess "http://localhost:3000/page1/page1.js"
    )
  , ( "second page"
    , \env -> withWreq env $ \sess -> do
        get sess "http://localhost:3000/page2/page2.css"
        get sess "http://localhost:3000/page2/page2.js"
    )
  ]

```

### Examples

`wrecker` is particularly useful for benchmarking a series of dependent
requests. `wrecker` includes a more complex typed API client example. The tutorial is
below, but you can run the examples with the following commands.

 - Run the client and server example with `cabal run example`
 - Run the client with `cabal run example-client `
 - Run the serve with `cabal run example-server`

# Create a Typed API Client with `wrecker`

Here is what a typed API client looks like.

### Example Script

```haskell
testScript :: Int -> Environment -> IO ()
testScript port env = withWreq env $ \sess -> do
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

### Output

Output from running

```bash
cabal build example -- --concurrency=1000 --interactive
```

![Example terminal output](/examples/new-example.gif?raw=true "Example Terminal Output")

### Full Tutorial

[Click here to see the full type API tutorial](/examples/Client.md)

### Memory Use

`wrecker` calculates statistics incrementally and is able to use a constant
amount of memory regardless of the length of time it is run.

Here is a heap snapshot for a thousand concurrent connections

![Heap Snapshot](/memoryProfile.png?raw=true "Heap Snapshot")
