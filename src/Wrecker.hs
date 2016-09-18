{-| 'wrecker' is a library for creating HTTP benchmarks. It is designed for
    benchmarking a series of HTTP request were the output of previous requests
    are used as inputs to the next request. This is useful for complex API 
    profiling situations. 

    'wrecker' does not provide any mechanism for making HTTP calls. It works
    with any HTTP client that produces a 'HttpException' during failure (so 
    http-client and wreq will work out of the box).
    
    See the documentation for examples of how to use 'wrecker' with 
    benchmarking scripts.
-}
module Wrecker ( Recorder
               , defaultMain 
               , record
               , run
               , Options (..)
               , defaultOptions
               ) where
import Wrecker.Recorder
import Wrecker.Main
import Wrecker.Options
import Wrecker.Runner