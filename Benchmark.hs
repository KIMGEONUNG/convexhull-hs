import System.IO (readFile)
import Control.Applicative ((<$>))
import Data.Maybe (isJust, fromJust)
import Data.List (sortBy)
import Data.Function (on)
import Data.Typeable

import Convexhull
import Geos

takeNum= 1000

data Entry = Entry { x :: Double, y:: Double} deriving Show
parseEntry (a:b:_) = Entry (read a :: Double) (read b :: Double)

entry2point :: Entry -> Point2d
entry2point (Entry x y) = Point x y 

parser = map entry2point . map parseEntry . map (take 2) . map (drop 1) . map words . take takeNum . drop 1 . lines  

main = 
    parser <$> readFile "N10000.txt" 
    >>= \pts -> giftWrapping <$> return pts 
    >>= \convexPts -> print convexPts


{-
main = parser                <$> readFile "N10000.txt" >>=  \pts -> giftWrapping                    <$> pure pts       >>=   \convexPts -> print convexPts
     = {String -> [Point2d]} <$> {IO String}           >>= (\pts -> {[Point2d] -> [Point2d]}        <$> pure pts       >>=  (\convexPts -> {IO ()}))
     = {IO [Point2d]}                                  >>= (\pts -> {[Point2d] -> [Point2d]}        <$> pure pts       >>=  (\convexPts -> {IO ()}))
     = {IO [Point2d]}                                  >>= ({[Point2d]} -> {[Point2d] -> [Point2d]} <$> {IO [Point2d]} >>= ({[Point2d]} -> {IO ()}))
     = {IO [Point2d]}                                                                                                  >>= ({[Point2d]} -> {IO ()})
     =                                                                                                                                     {IO ()}

*same with main = parser <$> readFile "N10000.txt" >>= \pts -> giftWrapping <$> pure pts >>= \convexPts -> print convexPts
-} 

