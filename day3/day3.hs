{-# LANGUAGE OverloadedStrings #-}

import Data.Ord (comparing)
import Data.Foldable (minimumBy)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Test.Hspec

data Direction = U | D | L | R 
  deriving (Eq, Show)

data Move = Move Direction Int
  deriving (Eq, Show)

data Point = Point Int Int
  deriving (Eq, Show)

-- A Segment is the distance to its end and the start and end points
data Segment = Segment Int Point Point
  deriving (Eq, Show)

-- An Intersection is the crossing point and the distance along each wire to that point
data Intersection = Intersection Point Int Int
  deriving (Show)

type Wire = [Segment]

manhattan :: Point -> Int
manhattan (Point x y) = (abs x) + (abs y)

centralPort :: Point
centralPort = Point 0 0

expandWire :: [Move] -> Wire
expandWire segs = tail segments
  where
    follow (Segment len _ (Point x y)) (Move U n) = Segment (len+n) (Point x (y+1)) (Point x (y+n))
    follow (Segment len _ (Point x y)) (Move D n) = Segment (len+n) (Point x (y-1)) (Point x (y-n))
    follow (Segment len _ (Point x y)) (Move L n) = Segment (len+n) (Point (x-1) y) (Point (x-n) y)
    follow (Segment len _ (Point x y)) (Move R n) = Segment (len+n) (Point (x+1) y) (Point (x+n) y)
    segments = scanl follow (Segment 0 centralPort centralPort) segs

load :: String -> [Wire]
load = map expandWire . map wire . T.splitOn "\n" . T.strip . T.pack
  where
    wire = map (segment . T.unpack . T.strip) . T.splitOn ","
    segment (d:len) = Move (dir d) (read len)
    dir 'U' = U
    dir 'D' = D
    dir 'L' = L
    dir 'R' = R 

vertical :: Segment -> Bool
vertical (Segment _ (Point x1 _) (Point x2 _)) = x1 == x2

horizontal :: Segment -> Bool
horizontal (Segment _ (Point _ y1) (Point _ y2)) = y1 == y2

wireIntersections :: Wire -> Wire -> [Intersection]
wireIntersections [] _ = []
wireIntersections _ [] = []
wireIntersections (x:xs) (y:ys) =
  maybeToList (intersection x y) ++ wireIntersections [x] ys ++ wireIntersections xs ys

crosses :: Int -> Int -> Int -> Bool
crosses a b c = (min a b) < c && c < (max a b)

intersection :: Segment -> Segment -> Maybe Intersection
intersection s1@(Segment l1 (Point x1 y1) (Point x2 y2)) s2@(Segment l2 (Point x3 y3) (Point x4 y4)) =
  if vertical s1 && horizontal s2 && crosses y1 y2 y3 && crosses x3 x4 x1 then
    Just $ Intersection (Point x1 y3) (l1-abs(y2-y3)) (l2-abs(x4-x1))
  else
  if horizontal s1 && vertical s2 && crosses x1 x2 x3 && crosses y3 y4 y1 then
    Just $ Intersection (Point x3 y1) (l1-abs(x2-x3)) (l2-abs(y4-y1))
  else
    Nothing

intersections :: [Wire] -> [Intersection]
intersections [] = []
intersections (w:ws) = (intersections' w ws) ++ (intersections ws)
  where
    intersections' w ws = concat $ map (wireIntersections w) ws

intersectionManhattan :: Intersection -> Int
intersectionManhattan (Intersection p _ _) = manhattan p

intersectionDistance :: Intersection -> Int
intersectionDistance (Intersection _ a b) = a + b

bestIntersectionBy :: (Intersection -> Int) -> [Intersection] -> Maybe Intersection
bestIntersectionBy _   [] = Nothing
bestIntersectionBy cmp is = Just $ minimumBy (comparing cmp) is

part1 :: [Wire] -> Maybe Int
part1 = fmap intersectionManhattan . bestIntersectionBy intersectionManhattan . intersections

part2 :: [Wire] -> Maybe Int
part2 = fmap intersectionDistance . bestIntersectionBy intersectionDistance . intersections

testCase1 = load "R8,U5,L5,D3\nU7,R6,D4,L4"
testCase2 = load "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
testCase3 = load "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

main = do
  part1 testCase1 `shouldBe` Just 6
  part1 testCase2 `shouldBe` Just 159
  part1 testCase3 `shouldBe` Just 135

  part2 testCase1 `shouldBe` Just 30
  part2 testCase2 `shouldBe` Just 610
  part2 testCase3 `shouldBe` Just 410

  input <- fmap load $ readFile "input.txt"
  putStrLn $ show (part1 input)
  putStrLn $ show (part2 input)
