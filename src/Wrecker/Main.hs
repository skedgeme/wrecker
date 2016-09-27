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

To see the options defaultMain can parse call `--help`

> $ wrecker-based-app --help
>
> wrecker - HTTP stress tester and benchmarker
>
> Usage: example [--concurrency ARG] [--bin-count ARG] ([--run-count ARG] |
>                [--run-timed ARG]) [--timeout-time ARG] [--display-mode ARG]
>                [--log-level ARG] [--match ARG] [--request-name-size ARG]
>                [--output-path ARG] [--silent]
>  Welcome to wrecker
>
> Available options:
>  -h,--help                Show this help text
>  --concurrency ARG        Number of threads for concurrent requests
>  --bin-count ARG          Number of bins for latency histogram
>  --run-count ARG          number of times to repeat
>  --run-timed ARG          number of seconds to repeat
>  --timeout-time ARG       How long to wait for all requests to finish
>  --display-mode ARG       Display results interactively
>  --log-level ARG          Display results interactively
>  --match ARG              Only run tests that match the glob
>  --request-name-size ARG  Request name size for the terminal display
>  --output-path ARG        Save a JSON file of the the statistics to given path
>  --silent                 Disable all output

-}
defaultMain :: [(String, Recorder -> IO ())] -> IO ()
defaultMain actions = void . flip run actions =<< runParser
