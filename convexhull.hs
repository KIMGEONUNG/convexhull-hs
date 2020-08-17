data Point2d = Point Double Double
data Vector2d = Vector Double Double
data Line = Line Point2d Point2d

instance Num Point2d where
    (+) (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2) 
    (-) (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2) 
    (*) (Point x1 y1) (Point x2 y2) = Point (x1 * x2) (y1 * y2) 
    negate (Point x y) = Point (-x) (-y) 
    abs (Point x y) = Point (abs x) (abs y) 
    signum (Point x y) = Point (signum x) (signum y) 
    fromInteger v = error "Not Implemented"

instance Num Vector2d where
    (+) (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2) 
    (-) (Vector x1 y1) (Vector x2 y2) = Vector (x1 - x2) (y1 - y2) 
    (*) (Vector x1 y1) (Vector x2 y2) = Vector (x1 * x2) (y1 * y2) 
    negate (Vector x y) = Vector (-x) (-y) 
    abs (Vector x y) = Vector (abs x) (abs y) 
    signum (Vector x y) = Vector (signum x) (signum y) 
    fromInteger v = error "Not Implemented"

instance Show Point2d where
    show (Point x y) = "X: " ++ show x ++ ", Y: " ++ show y

instance Show Vector2d where
    show (Vector x y) = "X: " ++ show x ++ ", Y: " ++ show y

instance Show Line where
    show (Line pt1 pt2) = "FROM: (" ++ show pt1 ++ "), TO: (" ++ show pt2 ++ ")" 

-- return radian value
vectorAngle :: Vector2d -> Vector2d -> Double
vectorAngle v1 v2 = acos $ dotProduct v1 v2 / vectorLength v1 / vectorLength v2 

vectorFromTwoPoints :: Point2d -> Point2d -> Vector2d
vectorFromTwoPoints (Point fromx fromy) (Point tox toy) = Vector (tox - fromx) (toy - fromy)

dotProduct :: Vector2d -> Vector2d -> Double
dotProduct (Vector x1 y1) (Vector x2 y2) = (x1 * x2) + (y1 * y2)

vectorLength :: Vector2d -> Double
vectorLength (Vector x y) = sqrt $ x * x + y * y

radianToDegree :: Double -> Double
radianToDegree r = 180.0 / pi * r 

degreeToRadian :: Double -> Double
degreeToRadian d = pi / 180.0 * d  

v1 = Vector 10 0
v2 = Vector 0 10
