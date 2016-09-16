module Wrecker.RunnerSpec where
import Wrecker.Runner
import Test.Hspec 

main :: IO ()
main = hspec spec

spec :: Spec 
spec = describe "Runner" $ do
  it "updateSampler correctly adds event" $ do
    False `shouldBe` True
  it "collectEvent collects all events until the recorder stops" $ do 
    False `shouldBe` True
  it "runWith works" $ do 
    False `shouldBe` True