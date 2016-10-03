{-| 'wrecker' is a library and executable for creating HTTP benchmarks. It is designed for
benchmarking a series of dependent requests.

'wrecker' includes a wrapped version of the `wreq` Session API
, mainly through 'Network.Wreq.Wrecker'.

import 'Network.Wreq.Wrecker' to write clients and 'Wrecker' to run the
them with either 'defaultMain' ir 'run'.
-}
module Wrecker ( Environment      (..)
               , Recorder
               , defaultMain
               , record
               , run
               , Options          (..)
               , URLDisplay       (..)
               , RunType          (..)
               , DisplayMode      (..)
               , defaultOptions
               , runParser
               , AllStats         (..)
               , ResultStatistics (..)
               , newStandaloneRecorder
               , runOne
               ) where
import Wrecker.Recorder
import Wrecker.Main
import Wrecker.Options
import Wrecker.Runner
import Wrecker.Statistics
