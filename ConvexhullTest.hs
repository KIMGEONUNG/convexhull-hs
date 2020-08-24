module ConvexhullTest where

import Convexhull
import Geos 

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "CONVEXHULL.HS MODULE UNIT TEST" $ do
        describe "find2ndPt function unit test" $ do
            it "test_001" $ do
                let pts = [ Point 1 1, Point 1 0, Point 0 1] 
                find2ndPt (Point 0.0 0.0) [ Point 1 1, Point 1 0, Point 0 1] [ Point 1 1, Point 1 0, Point 0 1] `shouldBe` (Point 0 1) 

        describe "findNextPt function unit test" $ do
            it "test_001" $ do
                findNextPt (Point 0.0 0.0)  (Point 0.0 1.0) (Point 0 0)  [ Point 0 0, Point 0 1, Point 0.5 2, Point 1 1, Point 1 0, Point 0 1] `shouldBe` [Point 0.5 2, Point 1 1, Point 1 0 ] 

        describe "giftWrapping function unit test" $ do
            it "test_001" $ do
                giftWrapping [Point 2 4, Point 5 13, Point 4 3, Point 13 5, Point 0 0, Point 8 4, Point 10 10, Point 3 2, Point 10 0, Point 4 9, Point 0 10] `shouldBe` [Point 0 0, Point 0 10, Point 5 13, Point 10 10, Point 13 5, Point 10 0]
            it "test_002" $ do
                giftWrapping [Point 0 0, Point 1 0, Point 0 1] `shouldBe` [Point 0 0, Point 0 1, Point 1 0]

