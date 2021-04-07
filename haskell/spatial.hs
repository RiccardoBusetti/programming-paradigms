module DatapointAggregation
where

import Data.List ( sortBy )
import Data.Function ( on )

-- Definition of types and list --
type Point = (Double,Double)
type Rectangle = (Point,Point)

myDatapoints :: [Point]
myDatapoints = [(2.3,5.4),(3.4,4.8),(6.3,9.4),(7.1,5.4),(1.1,8.5),(8.7,3.3),(9.3,2.3),(4.6,5.8),(7.6,4.9),(2.4,2.8),(3.9,1.1),(8.2,2.3),(4.4,7.2),(5.5,2.3),(9.1,9.8),(9.6,7.1)]
-- End of definitions --


-- Start of answers --
-- 1 --
boundingBox :: [Point] -> Rectangle -> [Point]
boundingBox points rectangle = filter (`isBounded` rectangle) points

-- 2 --
minDist :: Point -> [Point] -> Double
minDist point1 points = minimum (map (manhattanDistance point1) points)

-- 3 --
nearestNeighbors :: Point -> Int -> [Point] -> [Point]
nearestNeighbors sourcePoint k points = map fst (take k (sortBy (compare `on` snd) (sourcePoint `zipDistance` points)))

-- 4 --
nonDominated :: [Point] -> [Point]
nonDominated points = filter (`isNotDominated` points) points
-- End of answers --


-- Start of helper functions --
manhattanDistance :: Point -> Point -> Double
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- x1, y1 (bottom left) | x2, y2 (top right)
isBounded :: Point -> Rectangle -> Bool
isBounded (x, y) ((x1, y1), (x2, y2)) = x >= x1 && x <= x2 && y <= y2 && y >= y1

zipDistance :: Point -> [Point] -> [(Point, Double)]
zipDistance sourcePoint = map (\point2 -> (point2, manhattanDistance sourcePoint point2))

isNotDominated :: Point -> [Point] -> Bool
isNotDominated sourcePoint points = not (any (\point -> point /= sourcePoint && isDominant point sourcePoint) points)

isDominant :: Point -> Point -> Bool
isDominant (x1, y1) (x2, y2) = x1 < x2 && y1 < y2
-- End of helper functions --