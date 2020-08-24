module Convexhull where 

import Geos
import Utils

import Data.List
import Data.Ord

-- | The 'find2ndPt' function used in convexhull algorithm called gift wrapping. 
-- The first point on the convexhull is leftmost point. And the second point   
-- is on the location which every other points is on the same side.
find2ndPt :: Point2d -> [Point2d] -> [Point2d] -> Point2d
find2ndPt pt (target:pts1) pts2 =
    let oris = map (pointOrientation pt target) pts2 
    in if not $ any (==CCW) oris 
        then target 
        else find2ndPt pt pts1 pts2 

-- | The 'findNextPt' function used in convexhull algorithm called gift wrapping. 
-- The first point on the convexhull is leftmost point. And the second point   
-- is on the location which every other points is on the same side.
findNextPt :: Point2d -> Point2d -> Point2d -> [Point2d] -> [Point2d]
findNextPt pt1 pt2 end pts =
    let remains = removeAllElement pt2 pts
        polarAngle' x = polarAngle (vectorFromTwoPoints pt2 pt1) (vectorFromTwoPoints pt2 x)
        polarAngles = map polarAngle' remains
        max' = maximumBy (comparing fst) (zip polarAngles remains)
        target = snd max'
    in if target /= end 
            then target : findNextPt pt2 target end pts 
            else []

-- | The 'giftWrapping' function is convexhull algorithm known as Jarvis March 
giftWrapping :: [Point2d] -> [Point2d]
giftWrapping ([]) = error "Fail: Invalid point count" 
giftWrapping (_:[]) = error "Fail: Invalid point count" 
giftWrapping (_:_:[]) = error "Fail: Invalid point count" 
giftWrapping pts = 
    let distincts = nub pts 
        sorted = quicksort distincts 
        base = head sorted
        rest = tail sorted
        second = find2ndPt base rest rest
    in [ base, second ] ++ findNextPt base second base sorted 
