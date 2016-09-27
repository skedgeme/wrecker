module Wrecker.RecorderSpec where
-- import Wrecker.Recorder
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Recorder" $ do
  it "add/read event" $ do
    pending
  it "stop recorder closes the queue" $ do
    pending
  it "records success is timed correctly" $ do
    pending
  it "records status code failure correctly" $ do
    pending
  it "records totally failure correctly" $ do
    pending
