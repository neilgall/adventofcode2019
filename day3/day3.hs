{-# LANGUAGE OverloadedStrings #-}

import Data.Ord (comparing)
import Data.Foldable (minimumBy)
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Text as T
import Test.Hspec

data Direction = U | D | L | R 
  deriving (Eq, Show)

data Point = Point Int Int
  deriving (Eq, Show)

data SegmentDescriptor = SegmentDescriptor Direction Int
  deriving (Eq, Show)

data SegmentLine = SegmentLine Point Point
  deriving (Eq, Show)

type WireDescriptor = [SegmentDescriptor]
type Wire = [SegmentLine]

load :: String -> [WireDescriptor]
load = map wire . T.splitOn "\n" . T.strip . T.pack
  where
    wire = map (segment . T.unpack . T.strip) . T.splitOn ","
    segment (d:len) = SegmentDescriptor (dir d) (read len)
    dir 'U' = U
    dir 'D' = D
    dir 'L' = L
    dir 'R' = R 

centralPort :: Point
centralPort = Point 0 0

expandWire :: WireDescriptor -> Wire
expandWire segs = map toSegment (tail segments)
  where
    follow (_, Point x y) (SegmentDescriptor U n) = (Point x (y+1), Point x (y+n))
    follow (_, Point x y) (SegmentDescriptor D n) = (Point x (y-1), Point x (y-n))
    follow (_, Point x y) (SegmentDescriptor L n) = (Point (x-1) y, Point (x-n) y)
    follow (_, Point x y) (SegmentDescriptor R n) = (Point (x+1) y, Point (x+n) y)
    segments = scanl follow (centralPort, centralPort) segs
    toSegment (a, b) = SegmentLine a b

vertical :: SegmentLine -> Bool
vertical (SegmentLine (Point x1 _) (Point x2 _)) = x1 == x2

horizontal :: SegmentLine -> Bool
horizontal (SegmentLine (Point _ y1) (Point _ y2)) = y1 == y2

wireIntersections :: Wire -> Wire -> [Point]
wireIntersections [] _ = []
wireIntersections _ [] = []
wireIntersections (x:xs) (y:ys) = 
  maybeToList (intersection x y) ++ wireIntersections [x] ys ++ wireIntersections xs ys

intersection :: SegmentLine -> SegmentLine -> Maybe Point
intersection s1@(SegmentLine (Point x1 y1) (Point x2 y2)) s2@(SegmentLine (Point x3 y3) (Point x4 y4)) =
  if vertical s1 && horizontal s2 && crosses y1 y2 y3 && crosses x3 x4 x1
  then Just (Point x1 y3)
  else if horizontal s1 && vertical s2 && crosses x1 x2 x3 && crosses y3 y4 y1
  then Just (Point x3 y1)
  else Nothing

crosses :: Int -> Int -> Int -> Bool
crosses a b c = (min a b) < c && c < (max a b)

intersections :: [Wire] -> [Point]
intersections [] = []
intersections (w:ws) = (intersections' w ws) ++ (intersections ws)
  where
    intersections' w ws = concat $ map (wireIntersections w) ws

manhattan :: Point -> Int
manhattan (Point x y) = (abs x) + (abs y)

closestIntersection :: [Wire] -> Maybe Point
closestIntersection ws =
  let
    is = intersections ws
  in
    if null is then Nothing else Just $ minimumBy (comparing manhattan) is

part1 :: [WireDescriptor] -> Maybe Int
part1 = fmap manhattan . closestIntersection . map expandWire

test :: String -> Maybe Int
test = part1 . load

main = do
  test "R8,U5,L5,D3\nU7,R6,D4,L4" `shouldBe` Just 6
  test "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` Just 159
  test "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe` Just 135

  input <- fmap load $ readFile "input.txt"
  putStrLn $ show (part1 input)
