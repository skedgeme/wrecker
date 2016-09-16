module Wrecker.RecorderSpec where
import Wrecker.Recorder
import Test.Hspec 

main :: IO ()
main = hspec spec

spec :: Spec 
spec = describe "Recorder" $ do
  it "add/read event" $ do 
    False `shouldBe` True
  it "stop recorder closes the queue" $ do
    False `shouldBe` True
  it "records success is timed correctly" $ do
    False `shouldBe` True
  it "records status code failure correctly" $ do
    False `shouldBe` True
    
  it "records totally failure correctly" $ do 
    False `shouldBe` True
  
    