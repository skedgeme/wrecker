# Building a API client for profiling with `wrecker`

Unlike most HTTP benchmarking applications, `wrecker` is intended to benchmark
HTTP calls inline with other forms of processing. This allows for complex
interactions necessary to benchmark certain API endpoints.

## TL;DR

`wrecker` let's you build beautiful API clients that you can use for profiling

Here is the example we will build.

    testScript :: Int -> Recorder -> IO ()
    testScript port recorder = do
      Root { login
           , products
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

If this doesn't make sense on inspection, that is okay. This file builds up all
the necessary utilities and documents every line.

Most of the code in this file is "generic". It is the type of boilerplate you
make once for an API client.

You don't need to make a polish API client to use `wrecker`, just look at
TODO_MAKE_AESON_LENS_EXAMPLE to see how to use `record` with less setup.

## Boring Haskell Prelude

This is Haskell, so first we turn on the extensions we would like to use.

```haskell
{-# LANGUAGE NamedFieldPuns
           , DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings 
           , DuplicateRecordFields
#-}
module WreckerSpec where
```

`NamedFieldPuns` will let us destructure records conveniently. `DeriveAnyClass` and
`DeriveGeneric` are used turned on so the compiler can generate the JSON
conversion functions for us automatically.

Now we import the packages necessary to make the client.

### The Essence of `wrecker` is `record`

```haskell
import Wrecker (record, defaultMain, Recorder)
```

`record` is the primary function from `wrecker`. It has the signature

    record :: Recorder -> String -> IO a -> IO a

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

#### Other packages you can mostly ignore
```haskell
import GHC.Generics
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text as T
import Network.HTTP.Client (responseBody)
```

## Make a Somewhat Generic JSON API

`wreq` is pretty easy to use for JSON APIs but it could be easier. Here we make
a quick wrapper around `wreq` specialized to JSON and we utilize `record`

First, there is the ever popular `Envelope` type. On the right of the type
definition is the JSON serialization.

```haskell
data Envelope a = Envelope { value :: a } -- <=> -- {"value" : toJSON a}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
```

The `Envelope` only exists to transmit data between the server and the browser.

We unwrap values coming from the server in `Envelope`.
```haskell
fromEnvelope :: FromJSON a => IO (Wreq.Response ByteString) -> IO a
fromEnvelope x = fmap (value . responseBody) . Wreq.asJSON =<< x
```
We wrap values going to the server in an `Envelope`
```haskell
toEnvelope :: ToJSON a => a -> Value
toEnvelope = toJSON . Envelope
```
We can wrap functions too.
```haskell
liftEnvelope :: (ToJSON a, FromJSON b)
            => (Value -> IO (Wreq.Response ByteString))
            -> (a     -> IO b                         )
liftEnvelope f = fromEnvelope . f . toEnvelope
```

### Wrap HTTP Calls with `record`

We hide it's existence and specialize `wreq`'s' HTTP functions to operate on
our JSON API.

```haskell 
jsonGet :: FromJSON a => Recorder -> String -> Text -> IO a
jsonGet recorder key url = fromEnvelope $ record recorder key $ Wreq.get (T.unpack url)

jsonPost :: (ToJSON a, FromJSON b) => Recorder -> String -> Text -> a -> IO b
jsonPost recorder key url = liftEnvelope $ record recorder key . Wreq.post (T.unpack url)
```

## Make a Somewhat Generic REST API

### Resource References

Our client we will represent resource urls using the type `Ref`
```haskell
data Ref a = Ref { unRef :: Text }
  deriving (Show, Eq)
```

`Ref` is nothing more than a `Text` wrapper (the value there is the URL). `Ref`
has polymorphic `a` so we can talk about different types of resources.

A `FromJSON` instance which wraps a `Text` value, assuming the JSON is `Text`.

```haskell
instance FromJSON (Ref a) where
  parseJSON = withText "FromJSON (Ref a)" (return . Ref)
```

The `ToJSON` is just the reverse.

```haskell
instance ToJSON (Ref a) where
  toJSON (Ref x) = toJSON x 
```

In addition to resources our API has ad-hoc RPC calls. RPC calls are also
represented as a URL.

### Adhoc RPC

```haskell
data RPC a b = RPC Text
  deriving (Show, Eq)

instance FromJSON (RPC a b) where
  parseJSON = withText "FromJSON (Ref a)" (return . RPC)
```

### REST API Actions

We utilize our `jsonGet` and `jsonPost` functions and make specialized versions
for our more specific REST and RPC calls.

```haskell
get :: FromJSON a => Recorder -> String -> Ref a -> IO a
get recorder key (Ref url) = jsonGet recorder key url

insert :: (ToJSON a, FromJSON a) => Recorder -> String -> Ref [a] -> a -> IO (Ref [a])
insert recorder key (Ref url) = jsonPost recorder key url

rpc :: (ToJSON a, FromJSON b) => Recorder -> String -> RPC a b -> a -> IO b
rpc recorder key (RPC url) = jsonPost recorder key url
```

## The Example API

The API requires an initial call to the "/root" to obtain the URLs for
subsequent calls

```haskell
rootRef :: Int -> Ref Root
rootRef port = Ref $ T.pack $ "http://localhost:" ++ show port ++ "/root"
```

### API Response types

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
  { cart     :: Ref Cart                   -- <=> -- { "cart"     : "http://localhost:3000/carts/0"
  , username :: Text                       --     -- , "username" : "example"
  } deriving (Eq, Show, Generic, FromJSON) --     -- }
```

```haskell
data Credentials = Credentials 
  { password :: Text
  , username :: Text
  } deriving (Eq, Show, Generic, ToJSON)
```

## Profiling Script

We can now easily write our first script!

```haskell
testScript :: Int -> Recorder -> IO ()
testScript port recorder = do
```
Bootstrap the script and get all the URLs for the endpoints. Unpack `login`,
`products` and `checkout` for use later down.

```haskell
  Root { login, products, checkout } <- get recorder "root" (rootRef port)
```
We get all products and name the first one
```haskell
  firstProduct : _ <- get recorder "products" products
```

Login and get the user's ref.
```haskell
  userRef <- rpc recorder "login" login
                                  ( Credentials
                                     { username = "a@example.com"
                                     , password = "password"
                                     }
                                  )
```
Get the user and unpack the user's cart.
```haskell
  User { cart } <- get recorder "user" userRef
```
Get the cart unpack the items.
```haskell
  Cart { items } <- get recorder "cart" cart
```
Add the first product to the user's cart's items.
```haskell
  insert recorder "items" items firstProduct
```
Checkout.
```haskell
  rpc recorder "checkout" checkout cart
```

Port is hard coded to 3000 for this example

```haskell
main :: IO ()
main = defaultMain [("test0", testScript 3000)]
```
