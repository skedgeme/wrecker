module Wrecker.RunnerSpec where
-- import Wrecker.Runner
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Runner" $ do
  it "updateSampler correctly adds event" $ do
    pending
  it "collectEvent collects all events until the recorder stops" $ do
    pending
  it "runWith works" $ do
    pending
