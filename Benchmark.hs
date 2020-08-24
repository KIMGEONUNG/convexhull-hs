import System.IO (readFile)
import Control.Applicative ((<$>))
import Data.Maybe (isJust, fromJust)
import Data.List (sortBy)
import Data.Function (on)
import Data.Typeable

import Convexhull
import Geos

takeNum= 10000

data Entry = Entry { x :: Double, y:: Double} deriving Show
parseEntry (a:b:_) = Entry (read a :: Double) (read b :: Double)

entry2point :: Entry -> Point2d
entry2point (Entry x y) = Point x y 

pts = map entry2point . map parseEntry . map (take 2) . map (drop 1) . map words . take takeNum . drop 1 . lines <$> readFile "N10000.txt"

main = do
    convexPts <- giftWrapping <$> pts 
    putStrLn "Convexhull Points"
    print convexPts 
