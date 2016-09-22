module Wrecker.Main (defaultMain) where
import Wrecker.Runner  (run)
import Wrecker.Options (runParser)
import Wrecker.Recorder (Recorder)
import Control.Monad (void)

{- | 'defaultMain' is typically the main entry point for 'wrecker' benchmarks.
     'defaultMain' will parse all command line arguments and then call 'run'
     with the correct 'Options'.

> import Wrecker
> import Your.Performance.Scripts (landingPage, purchase)
>
> main :: IO ()
> main = defaultMain
>  [ ("loginReshare", loginReshare)
>  , ("purchase"    , purchase    )
>  ]
-}
defaultMain :: [(String, Recorder -> IO ())] -> IO ()
defaultMain actions = void . flip run actions =<< runParser
