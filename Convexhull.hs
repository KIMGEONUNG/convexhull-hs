module Convexhull where 

--------------------------------------------------------------
----BEGIN DEFINITIONS FOR "PointBased"------------------------

class PointBased p where 
    ptCnt :: p -> Int

----END DEFINITIONS FOR "PointBased"--------------------------
--------------------------------------------------------------

----------------------------------------------------------------
----BEGIN DEFINITIONS FOR "Point2d"-----------------------------

data Point2d = Point Double Double

instance Num Point2d where
    (+) (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2) 
    (-) (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2) 
    (*) (Point x1 y1) (Point x2 y2) = Point (x1 * x2) (y1 * y2) 
    negate (Point x y) = Point (-x) (-y) 
    abs (Point x y) = Point (abs x) (abs y) 
    signum (Point x y) = Point (signum x) (signum y) 
    fromInteger v = error "Not Implemented"

instance Eq Point2d where
    (==) (Point x1 y1) (Point x2 y2) = (x1 == x2) && (y1 == y2) 
    (/=) (Point x1 y1) (Point x2 y2) = (x1 /= x2) || (y1 /= y2) 

instance Ord Point2d where
    compare (Point x1 y1) (Point x2 y2) 
        | (x1 == x2) && (y1 == y2) = EQ
        | (x1 < x2)                = LT 
        | (x1 == x2) && (y1 < y2)  = LT 
        | (x1 > x2)                = GT 
        | (x1 == x2) && (y1 > y2)  = GT 
        | otherwise = error "Invalid case"
    (<) pt1 pt2 = compare pt1 pt2 == LT
    (<=) pt1 pt2 = a == LT || a == EQ
                where a = compare pt1 pt2
    (>) pt1 pt2 = compare pt1 pt2 == GT 
    (>=) pt1 pt2 = a == GT || a == EQ
                where a = compare pt1 pt2
    max pt1 pt2  
        | pt1 <= pt2 = pt2
        | otherwise = pt1
    min pt1 pt2  
        | pt1 >= pt2 = pt2
        | otherwise = pt1

instance Show Point2d where
    show (Point x y) = "X: " ++ show x ++ ", Y: " ++ show y

instance PointBased Point2d where
    ptCnt pt = 1

----END DEFINITIONS FOR "Point2d"-----------------------------
--------------------------------------------------------------

--------------------------------------------------------------
----BEGIN DEFINITIONS FOR "Vector2d"--------------------------
data Vector2d = Vector Double Double

instance Num Vector2d where
    (+) (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2) 
    (-) (Vector x1 y1) (Vector x2 y2) = Vector (x1 - x2) (y1 - y2) 
    (*) (Vector x1 y1) (Vector x2 y2) = Vector (x1 * x2) (y1 * y2) 
    negate (Vector x y) = Vector (-x) (-y) 
    abs (Vector x y) = Vector (abs x) (abs y) 
    signum (Vector x y) = Vector (signum x) (signum y) 
    fromInteger v = error "Not Implemented"

instance Eq Vector2d where
    (==) (Vector x1 y1) (Vector x2 y2) = (x1 == x2) && (y1 == y2) 
    (/=) (Vector x1 y1) (Vector x2 y2) = (x1 /= x2) || (y1 /= y2) 

instance Ord Vector2d where
    compare (Vector x1 y1) (Vector x2 y2) 
        | (x1 == x2) && (y1 == y2) = EQ
        | (x1 < x2)                = LT 
        | (x1 == x2) && (y1 < y2)  = LT 
        | (x1 > x2)                = GT 
        | (x1 == x2) && (y1 > y2)  = GT 
        | otherwise = error "Invalid case"
    (<) vec1 vec2 = compare vec1 vec2 == LT
    (<=) vec1 vec2 = a == LT || a == EQ
                where a = compare vec1 vec2
    (>) vec1 vec2 = compare vec1 vec2 == GT 
    (>=) vec1 vec2 = a == GT || a == EQ
                where a = compare vec1 vec2
    max vec1 vec2  
        | vec1 <= vec2 = vec2
        | otherwise = vec1
    min vec1 vec2  
        | vec1 >= vec2 = vec2
        | otherwise = vec1

instance Show Vector2d where
    show (Vector x y) = "X: " ++ show x ++ ", Y: " ++ show y

----END DEFINITIONS FOR "Vector2d"----------------------------
--------------------------------------------------------------

--------------------------------------------------------------
----BEGIN DEFINITIONS FOR "Line"------------------------------

data Line = Line Point2d Point2d

instance Show Line where
    show (Line pt1 pt2) = "FROM: (" ++ show pt1 ++ "), TO: (" ++ show pt2 ++ ")" 

----END DEFINITIONS FOR "Line"--------------------------------
--------------------------------------------------------------


-- | The 'vectorAngle' function retrive radian angle from two Vector2d
-- two vector argumet is commutative, that is, it always retrive positive angle
-- from 0 to 180 degree 
vectorAngle :: Vector2d -> Vector2d -> Double
vectorAngle v1 v2 = acos $ dotProduct v1 v2 / vectorLength v1 / vectorLength v2 

-- | The 'vectorFromTwoPoints' function retrive Vector from Two points.
-- The first arument is start point of result vector and the last is
-- end of result vector
vectorFromTwoPoints :: Point2d -> Point2d -> Vector2d
vectorFromTwoPoints (Point fromx fromy) (Point tox toy) = Vector (tox - fromx) (toy - fromy)

dotProduct :: Vector2d -> Vector2d -> Double
dotProduct (Vector x1 y1) (Vector x2 y2) = x1 * x2 + y1 * y2

crossProduct :: Vector2d -> Vector2d -> Double
crossProduct (Vector x1 y1) (Vector x2 y2) = x1 * y2 - y1 * x2

vectorLength :: Vector2d -> Double
vectorLength (Vector x y) = sqrt $ x * x + y * y

radianToDegree :: Double -> Double
radianToDegree r = 180.0 / pi * r 

degreeToRadian :: Double -> Double
degreeToRadian d = pi / 180.0 * d  

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs


-- | The 'Orientation' data type consists of 
-- Clockwise(CW), CounterClockwise(CCW), Colinear(CL) 
data Orientation = CW | CCW | CL
--
--pointOrientation :: Point2d -> Poin2d -> Point2d -> Orientation  

--giftWrapping :: [Point2d] -> [Point2d]

v1 = Vector 10 0
v2 = Vector 0 10

pt1 = Point 10 0
pt2 = Point 0 10
pts = [ Point 1 0
      , Point 0 0
      , Point 10 10
      , Point 0 10
      , Point 10 0
      , Point 2 1
      , Point 2 0 ]

