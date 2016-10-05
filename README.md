[![Hackage](https://img.shields.io/hackage/v/wrecker.svg)](https://hackage.haskell.org/package/wrecker)
[![Travis CI Status](https://travis-ci.org/skedgeme/wrecker.svg?branch=master)](http://travis-ci.org/skedgeme/wrecker)


### `wrecker`
`wrecker` is an HTTP benchmarking library and executable for profiling several API actions.

### Why?

There are plenty of HTTP profilers in existence, `wrk`, `ab`, `JMeter`, `LoadRunner`, etc. Most profilers provide some facility for scripting and using the responses of the previous requests for future requests. However, the scripting facility is usually weak and esoteric.

`wrecker` is designed from the ground up for scripting complex API sequences sublimely. Benchmarks can utilize a `wreq` like interface, perhaps the easiest to use Haskell library for HTTP interaction, and quickly create wonderful typed API clients.

#### How does it compare to my current http profiling?

If you are happy with your HTTP profiling setup, then `wrecker` doesn't offer
a reason to switch ... it does have an interactive mode.

![Wrecker Interactive Command Line](/wreck-interactive.gif)

That makes it a little easier to know when to stop.

### Quick Start

`wrecker` provides a simple executable [`wreck`](/app/Main.hs) which takes single URL to profile.

```
$ wreck http://localhost:3000/root
```

![wreck terminal output](/wreck-example.gif?raw=true "wreck Terminal Output")

The `wreck` executable is not that interesting. `wrecker` is more interesting
when used as a library to create a suite of benchmarks.

Similar functionality to `wreck` can be executed from ghci:

```bash
$ ghci
> import Wrecker
> import Network.Wreq.Wrecker
> runOne defaultOptions $ withWreq $ \sess -> get sess "http://localhost:3000/root"
```

Running with ghci is okay to get a feel for `wrecker` but it is recommended
that all benchmarks are compiled with optimizations, the threaded library,
and run with the RTS options `-N -I0 -qg`.

#### Accuracy

`wreck` produces results that are close to `wrk` and `ab` when the number of
connections are below 100. As the number of connections increases to 1000 and
10000 `wrk` continue to work well, but `wrecker` and `ab` produce inflated numbers.

You can play around with comparing `wreck` to `wrk` and `ab` in vm with the provide Vagrant file.

```bash
vagrant up && vagrant ssh
cd /vagrant && cabal run example-server -- 10000
```

The `100000` is the `threadDelay` for the requests in microseconds.

##### Example Results
 - 100 Connections
   - `wrk -d 10 -t 2 -c 100 http://localhost:3000/root`
     - mean: 104.78 ms    
     - variance: 0.009 ms
   - `ab -t 10 -c 100 http://localhost:3000/root`
     - mean: 106.9 ms
     - variance: 0.05 ms
   - `wreck --concurrency=100 --run-timed=10 http://localhost:3000/root`
     - mean: 105.6 ms
     - variance: 000.17 ms
##### Example Results 100 Connections
 - 1000 Connections
   - `wrk -d 10 -t 2 -c 100 http://localhost:3000/root`
     - mean: 135.42 ms
     - variance: 0.009 ms
   - `ab -t 10 -c 100 http://localhost:3000/root`
     - mean: 218.61
     - variance: 235.2 ms
   - `wreck --concurrency=100 --run-timed=10 http://localhost:3000/root`
     - mean: 316.0 ms
     - variance: 000.90 ms


### A More Complicated Benchmark

In addition to benchmarking a single URL, the `wrecker` library can create
a suite of benchmarks, each of which can contain multiple endpoints to
profile.

The example below uses the `wrecker`'s `wreq` interface but any `http-client`
based library could be used.

```haskell
import Wrecker
import Network.Wreq.Wrecker

main = defaultMain
  [ ( "first page"
    , withWreq $ \sess do
        get sess "http://localhost:3000/page1/page1.css"
        get sess "http://localhost:3000/page1/page1.js"
    )
  , ( "second page"
    , withWreq $ \sess do
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
testScript port = withWreq $ \sess -> do
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
