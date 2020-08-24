module UtilsTest where

import Utils 

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "UTILSTEST.HS MODULE UNIT TEST" $ do
        describe "removeElement function unit test" $ do
            it "test_001" $ do
                removeElement 3 [1,2,3,4] `shouldBe` [1,2,4]

        describe "removeAllElement function unit test" $ do
            it "test_001" $ do
                removeAllElement 3 [1,3,2,3,4,3] `shouldBe` [1,2,4]
