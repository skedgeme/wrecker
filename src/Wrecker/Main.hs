module Wrecker.Main where
import Wrecker.Runner  (run)
import Wrecker.Options (runParser)
import Wrecker.Recorder (Recorder)

defaultMain :: [(String, Recorder -> IO ())] -> IO ()
defaultMain actions = flip run actions =<< runParser