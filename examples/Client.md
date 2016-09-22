# Building a API client for profiling with `wrecker`

Unlike most HTTP benchmarking applications, `wrecker` is intended to benchmark
HTTP calls inline with other forms of processing. This allows for complex
interactions necessary to benchmark certain API endpoints.

## TL;DR

`wrecker` let's you build beautiful API clients that you can use for profiling

Here is the example we will build.

    testScript :: Port -> Recorder -> IO ()
    testScript port recorder = do
      let get    = getWithRecorder    recorder
          insert = insertWithRecorder recorder
          rpc    = rpcWithRecorder    recorder

      Root { login
           , products
           , checkout
           }             <- get "root"     (rootRef port)
      firstProduct : _   <- get "products" products
      userRef            <- rpc "login"    login
                                           ( Credentials
                                               { userName = "a@example.com"
                                               , password = "password"
                                               }
                                           )
      User { usersCart } <- get "user"     userRef
      Cart { items }     <- get "cart"     usersCart

      insert "items" items firstProduct
      rpc "checkout" checkout cart

If this doesn't make sense on inspection, that is okay. This file builds up all
the necessary utilities and documents every line.

Most of the code in this file is "generic". It is the type of boilerplate you
make once for an API client.

You don't need to make a polish API client to use `wrecker`, just look at
TODO_MAKE_AESON_LENS_EXAMPLE to see how to use `record` with less setup.

## Boring Haskell Prelude

This is Haskell, so first we turn on the extensions we would like to use.

```haskell
{-# LANGUAGE RecordPuns, DeriveAny, DeriveGeneric #-}
module WreckerSpec where
```

`RecordPuns` will let us destructure records conveniently. `DeriveAny` and
`DeriveGeneric` are used turned on so the compiler can generate the JSON
conversion functions for us automatically.

Now we import the packages necessary to make the client.

```haskell
import Wrecker (record, defaultMain)
```

`record` is the primary function from `wrecker`. It has the signature
```haskell
record :: Recorder -> String -> IO a -> IO a
```
`record` takes a `Recorder` and key in the form of a `String` and wraps some
`IO` action. `record` runs the passed in `IO a` and um ... records information
about such as the elapsed time and whether it succeeded or failed.


`defaultMain` is one of two entry points `wrecker` provides (the other is
`run`). `defaultMain` performs command line argument parsing for us, and
runs the benchmarks with the provided options. Additionally, `defaultMain`
creates a `Recorder` that is used by all the benchmark scripts.

```haskell
import Data.Aeson
```
We need JSON so of course we are using `aeson`.

```haskell
import qualified Network.Wreq as Wreq
```
`wrecker` does not provide any means for making HTTP calls. It records data,
computes statistics, controls concurrency and provides a convenient UI.
We leverage `wreq` to do the actual HTTP calls.

Here we wrap `wreq`'s `get` and `post` calls and make new functions which take
a `Recorder` so we can benchmark the times.

## Make a Some What Generic JSON API

`wreq` is pretty easy to use for JSON APIs but it could be easier. Here we make
a quick wrapper around `wreq` specialized to JSON and we utilize `wrecker`

First, there is the ever popular `Envelope` type. On the right of the type
definition is the JSON serialization.

```haskell
data Envelope a = Envelope { value :: a } -- <=> -- {"value" : toJSON a}
  deriving (Show, Eq, Generic, FromJSON)
```

The `Envelope` only exists to transmit data between the server and the browser.

We unwrap values coming from the server in `Envelope`.
```haskell
fromEnvelope :: FromJSON a => IO (Wreq.Response ByteString) -> IO a
fromEnvelope x = fmap unEnvelope =<< asJSON =<< x
```
We wrap values going to the server in an `Envelope`
```haskell
toEnvelope :: ToJSON a => Value
toEnvelope = toJSON . Envelope
```
We can wrap functions too.
```
liftEnvelop :: (ToJSON a, FromJSON b)
            => (Value -> IO (Wreq.Response ByteString))
            -> (a     -> IO b)
liftEnvelop f = fromEnvelope . f . toEnvelope
```

### Wrap HTTP Calls with `record`

We hide it's existence and specialize `wreq`'s' HTTP functions to operate on
our JSON API.
```
jsonGet :: FromJSON a => Recorder -> String -> String -> IO a
jsonGet recorder key = fromEnvelope $ record recorder key . Wreq.get

jsonPost :: (ToJSON a, FromJSON b) => Recorder -> String -> String -> a -> IO b
jsonPost recorder key url = liftEnvelope $ record recorder key . Wreq.post url
```

Alright now it is time to make our client for our toy API.

Our client we will represent resource urls using the type `Ref`
```haskell
data Ref a = Ref { unRef :: Text }
  deriving (Show, Eq)
```

## Make a Somewhat Generic REST API

`Ref` is nothing more than a `Text` wrapper (the value there is the URL). `Ref`
has polymorphic `a` so we can talk about different types of resources. It's use
will become clearer later on.

A `FromJSON` instance which wraps a `Text` value, assuming the JSON is `Text`.

```haskell
instance FromJSON (Ref a) where
  parseJSON = withText "FromJSON (Ref a)" Ref
```

In addition to resources our API has ad-hoc RPC calls. RPC calls are also
represented as a URL.

```haskell
data RPC a b = RPC String
  deriving (Show, Eq)

instance FromJSON (RPC a b) where
  parseJSON = withText "FromJSON (Ref a)" RPC
```

We utilize our `jsonGet` and `jsonPost` functions and make specialized versions.

```haskell

getWithRecorder :: FromJSON a => Recorder -> String -> Ref a -> IO a
getWithRecorder recorder key (Ref url) = jsonGet recorder key url

insertWithRecorder :: (ToJSON a, FromJSON a) => Recorder -> String -> Ref [a] -> a -> IO (Ref [a])
insertWithRecorder recorder key (Ref url) = jsonPost recorder key url

rpcWithRecorder :: (ToJSON a, FromJSON b) => Recorder -> String -> RPC a b -> a -> IO b
rpcWithRecorder recorder key (RPC url) = jsonPost recorder key url
```

## The Example API

The API requires an initial call to the "/root" to obtain the URLs for subsequent calls

```haskell
rootRef :: Port -> Ref Root
rootRef port = Ref $ "http://localhost:" ++ show port ++ "/root"
```

    Calling `GET` on "/root" returns the following JSON  ----------
                                                                  |
    Represented here --                                           |
                      |                                           |
                      v                                           v
```haskell                                                
data Root = Root                           
  { products :: Ref [Ref Product]          --     -- { "products" : "http://localhost:3000/products"
  , carts    :: Ref [Ref Cart   ]          -- <=> -- , "carts"    : "http://localhost:3000/carts"
  , users    :: Ref [Ref User   ]          --     -- , "users"    : "http://localhost:3000/users"
  , login    :: RPC Credentials (Ref User) --     -- , "login"    : "http://localhost:3000/login"
  , checkout :: RPC (Ref Cart)  ()         --     -- , "checkout" : "http://localhost:3000/checkout"
  } deriving (Eq, Show, Generic, FromJSON) --     -- }
```

Since the JSON is so uniform, we can use `aeson`s generic instances.

Calling `GET` on a `Ref Product` or "/products/:id" gives

```haskell
data Product = Product                     --     --
  { summary :: Text                        -- <=> -- { "summary" : "shirt" }
  } deriving (Eq, Show, Generic, FromJSON) --     --
```

Calling `GET` on a `Ref Cart` or "/carts/:id" gives

```haskell
data Cart = Cart                           --     --
  { items :: Ref [Ref Product]             -- <=> -- { "items" : ["http://localhost:3000/products/0"] }
  } deriving (Eq, Show, Generic, FromJSON) --     --
```

Calling `GET` on a `Ref User` or "/users/:id" gives

```haskell
data User = User                           --     --  
  { cart        :: Ref Cart                -- <=> -- { "cart"        : "http://localhost:3000/carts/0"
  , credentials :: Credentials             --     -- , "credentials" : { "user-name" : "example"
                                           --     --                 , "password"  : "password"
                                           --     --                 }
  } deriving (Eq, Show, Generic, FromJSON) --     -- }
```

## Profiling Script

We can now easily write our first script!

```haskell
testScript :: Port -> Recorder -> IO ()
testScript port recorder = do
```
First we make some copies of our api functions with `Recorder` partially
applied.

```haskell
  let get    = getWithRecorder    recorder
      insert = insertWithRecorder recorder
      rpc    = rpcWithRecorder    recorder
```

Now we can use the copies without threading the recorder everywhere.

Bootstrap the script and get all the urls for the endpoints. Unpack `login` and
`products`.

```haskell
  Root { login, products } <- get "root" (rootRef port)
```
We get all products and name the first one
```haskell
  firstProduct : _ <- get "products" products
```

Login and get the user's ref.
```haskell
  userRef <- rpc "login" login
                          ( Credentials
                             { userName = "a@example.com"
                             , password = "password"
                             }
                          )
```
Get the user and unpack the user's cart.
```haskell
  User { usersCart } <- get "user" userRef
```
Get the cart unpack the items.
```haskell
  Cart { items } <- get "cart" usersCart
```
Add the first product to the user's cart's items.
```haskell
  insert "items" items firstProduct
```
Checkout.
```haskell
  rpc "checkout" checkout cart
```

Port is hard coded to 3000 for this example

```haskell
main :: IO ()
main = defaultMain [("test0", testScript 3000 port)]
```
