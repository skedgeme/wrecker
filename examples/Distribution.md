```haskell
{-# LANGUAGE LambdaCase, NamedFieldPuns #-}
module Distribution where
import System.Random.MWC
import System.Random.MWC.Distributions
```
Support a handful of distributions
```haskell
data Distribution
  = Gaussian    { mean     :: Double
                , variance :: Double
                }
  | Exponential { mean :: Double }
  | Uniform     { mean :: Double }
  | Constant    Double
  deriving (Show, Eq)
```

Sampling is evaluation

```haskell
sample :: GenIO
       -- ^ The random number generators state
       -> Distribution
       -- ^ The `Distribution` to sample from
       -> IO Double
sample gen = \case
  Gaussian    { mean     
              , variance
              }           -> normal      mean variance gen
  Exponential { mean }    -> exponential (1.0 / mean)  gen 
  Uniform     { mean }    -> fmap (mean *) $ uniform gen 
  Constant    x           -> return x
```


