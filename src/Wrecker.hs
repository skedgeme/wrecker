{-| 'wrecker' is a library and executable for creating HTTP benchmarks. It is designed for
benchmarking a series of dependent requests.

'wrecker' includes a wrapped version of the `wreq` Session API
, mainly through 'Network.Wreq.Wrecker'.

import 'Network.Wreq.Wrecker' to write clients and 'Wrecker' to run the
them with either 'defaultMain' or 'run'.

See https://github.com/skedgeme/wrecker#readme for more information.
-}
module Wrecker (-- * Entry Points
                 defaultMain
               , run
               , runOne
               -- * Wrecker State
               , Environment      (..)
               -- * Recorder
               , Recorder
               , record
               -- * Options
               , Options          (..)
               , URLDisplay       (..)
               , RunType          (..)
               , DisplayMode      (..)
               , defaultOptions
               -- Output Statistics
               , AllStats         (..)
               , ResultStatistics (..)


               ) where
import Wrecker.Recorder
import Wrecker.Main
import Wrecker.Options
import Wrecker.Runner
import Wrecker.Statistics
