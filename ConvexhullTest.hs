module ConvexhullTest where

import Convexhull

import Test.Hspec
import Test.QuickCheck

dotProductTest :: Vector2d -> Vector2d -> Double -> Spec
dotProductTest vec1 vec2 val =
    it "dotProduct function unit test" $
        dotProduct vec1 vec2 `shouldBe` val 

--main = hspec $ do
--    describe "dotProduct" $ do
--        dotProductTest (Vector 1.0 2.0) (Vector 3.0 4.0) 11.0
--        dotProductTest (Vector 0.0 2.0) (Vector 3.0 0.0) 0.0

main :: IO ()
main = hspec $ do
    describe "CONVEXHULL.HS MODULE UNIT TEST" $ do
        describe "vectorFromTwoPoints function unit test" $ do
            it "test_001" $ do
                vectorFromTwoPoints (Point 1.0 0.0) (Point 0.0 1.0) `shouldBe` (Vector (-1.0) 1.0) 
        describe "dotProduct function unit test" $ do
            it "test_001" $ do
                dotProduct (Vector 1.0 2.0) (Vector 3.0 4.0) `shouldBe` 11.0
            it "test_002" $ do
                dotProduct (Vector 0.0 2.0) (Vector 3.0 0.0) `shouldBe` 0.0
        describe "crossProduct function unit test" $ do
            it "test_001" $ do
                crossProduct (Vector 1.0 2.0) (Vector 3.0 4.0) `shouldBe` (-2) 
            it "test_002" $ do
                crossProduct (Vector 0.0 2.0) (Vector 3.0 0.0) `shouldBe` (-6) 
        describe "vectorAngle function unit test" $ do
            it "test_001" $ do
                vectorAngle (Vector 1.0 0.0) (Vector 0.0 1.0) `shouldBe` (pi / 2) 
            it "test_002" $ do
                vectorAngle (Vector 0.0 1.0) (Vector 1.0 0.0) `shouldBe` (pi / 2) 

