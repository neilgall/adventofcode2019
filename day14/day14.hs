import Data.Char (isAsciiUpper, isDigit, isSpace)
import Data.List (intersperse, partition, stripPrefix)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Prelude hiding (sequence)
import Test.Hspec hiding (before)
import qualified Data.Map as M

-- Simple parser combinators

data ParseResult input value
  = Ok value input
  | Err String input
  deriving (Eq, Show)

instance Functor (ParseResult input) where
  fmap f (Ok value rest) = Ok (f value) rest
  fmap f (Err expected actual) = Err expected actual

newtype Parser input value = Parser (input -> ParseResult input value)

instance Functor (Parser input) where
  fmap f (Parser p) = Parser (\input -> fmap f (p input))

instance Applicative (Parser input) where
  pure x = Parser (\input -> Ok x input)
  (Parser p) <*> (Parser q) = Parser $ \input ->
    case p input of
      Ok r rest -> fmap r (q rest)
      Err e a -> Err e a

parse :: Parser input value -> input -> ParseResult input value
parse (Parser p) input = p input

parseWith :: (Char -> Bool) -> (String -> a) -> String -> Parser String a
parseWith match convert expected = Parser $ \input ->
  let
    matching = takeWhile match input
    rest = dropWhile match input
  in
    if null matching 
      then Err expected input
      else Ok (convert matching) rest

literal :: String -> Parser String ()
literal s = Parser $ \input -> 
  case stripPrefix s input of
    Nothing -> Err ("'" ++ s ++ "'") input
    (Just rest) -> Ok () rest

integer :: Parser String Int
integer = parseWith isDigit read "an integer"

chemical :: Parser String String
chemical = parseWith isAsciiUpper id "a chemical symbol"

whitespace :: Parser String ()
whitespace = Parser $ \input -> Ok () (dropWhile isSpace input)

before :: Parser i x -> Parser i a -> Parser i a
x `before` p = fmap snd $ (,) <$> x <*> p

followedBy :: Parser i a -> Parser i x -> Parser i a
p `followedBy` x = fmap fst $ (,) <$> p <*> x

sepBy :: Parser i a -> Parser i s -> Parser i [a]
sepBy (Parser p) (Parser q) = Parser sepBy'
  where
    sepBy' input = case p input of
      Err _ _ -> Ok [] input
      Ok v rest -> case q rest of
        Err _ _ -> Ok [v] rest
        Ok _ rest' -> fmap (\vs -> v:vs) (sepBy' rest')


testParserCombinators = do
  parse integer "foo" `shouldBe` Err "an integer" "foo"
  parse integer "123abc" `shouldBe` Ok 123 "abc"

  parse chemical "ABCD" `shouldBe` Ok "ABCD" ""
  parse chemical "ABCD XYZ" `shouldBe` Ok "ABCD" " XYZ"
  parse chemical "123" `shouldBe` Err "a chemical symbol" "123"

  parse whitespace "XYZ" `shouldBe` Ok () "XYZ"
  parse whitespace " XYZ" `shouldBe` Ok () "XYZ"
  parse whitespace "  XYZ" `shouldBe` Ok () "XYZ"

  parse (literal "foo") "foobar" `shouldBe` Ok () "bar"
  parse (literal "foo") "barfoo" `shouldBe` Err "'foo'" "barfoo"

  parse (literal "foo" `before` integer) "foo123bar" `shouldBe` Ok 123 "bar"
  parse (integer `followedBy` literal "foo") "123foobar" `shouldBe` Ok 123 "bar"

  parse (integer `sepBy` (literal ",")) "1,2,3,4foo" `shouldBe` Ok [1,2,3,4] "foo"


-- Data model

type Name = String

data Material = Material Int Name
  deriving (Eq, Ord, Show)

data Reaction = Reaction [Material] Material
  deriving (Eq)

instance Show Reaction where
  show (Reaction inputs output) = inputs' ++ " => " ++ output'
    where
      inputs' = concat $ intersperse "," (map show inputs)
      output' = show output


-- Input parser

material :: Parser String Material
material = Material <$> (integer `followedBy` whitespace) <*> chemical

reaction :: Parser String Reaction
reaction = Reaction <$> inputs <*> output
  where
    inputs = material `sepBy` literal ", "
    output = literal " => " `before` material

reactions :: Parser String [Reaction]
reactions = reaction `sepBy` whitespace


testModelParser = do
  parse material "54 FUEL" `shouldBe` Ok (Material 54 "FUEL") ""
  parse reaction "8 A, 1 B => 1 C" `shouldBe` Ok (Reaction [Material 8 "A", Material 1 "B"] (Material 1 "C")) ""



-- Topological sort

data Edge = Edge Name Name

findEdges :: [Reaction] -> [Edge]
findEdges [] = []
findEdges ((Reaction inputs output):rs) = (map toEdge inputs) ++ (findEdges rs)
  where
    edgeOutput = (\(Material _ o) -> o) output
    toEdge (Material _ i) = Edge i edgeOutput


topoSort :: [Reaction] -> [Name]
topoSort rs = reverse $ topoSort' (findEdges rs) ["FUEL"] []
  where
    input (Edge i _) = i
    output (Edge _ o) = o
    from x e = input e == x
    to x e = output e == x
    noneFrom es e = not (any (from (input e)) es)

    topoSort' :: [Edge] -> [Name] -> [Name] -> [Name]
    topoSort' _ [] result = result
    topoSort' edges (here:stack) result =
      let
        (incoming, edges') = partition (to here) edges
        next = map input $ filter (noneFrom edges') incoming
        stack' = stack ++ next
      in
        topoSort' edges' stack' (here:result)


requirements :: [Reaction] -> M.Map Name (Int, [Material])
requirements [] = M.empty
requirements ((Reaction inputs (Material quantity name)):rs) = 
  M.insert name (quantity, inputs) (requirements rs)


quantitiesNeeded :: [Reaction] -> Int -> M.Map String Int
quantitiesNeeded rs fuel = foldl quantityNeeded (M.fromList [("FUEL", fuel)]) (topoSort rs)
  where
    add q Nothing = Just q
    add q (Just q') = Just (q' + q)
    reqs = requirements rs

    quantityNeeded :: M.Map String Int -> String -> M.Map String Int
    quantityNeeded neededByName name =
      case M.lookup name reqs of
        Nothing -> neededByName
        Just (makesQuantity, inputs) -> foldl addNeeded neededByName' inputs
          where
            Just needQuantity = M.lookup name neededByName
            scale = (needQuantity `div` makesQuantity) + (if needQuantity `mod` makesQuantity > 0 then 1 else 0)
            neededByName' = M.alter (add needQuantity) name neededByName
            addNeeded n (Material q m) = M.alter (add (scale * q)) m n


oreNeededForFuel :: [Reaction] -> Int -> Int
oreNeededForFuel rs fuel = fromMaybe 0 $ M.lookup "ORE" $ quantitiesNeeded rs fuel


maxFuelProduced :: [Reaction] -> Int -> Int
maxFuelProduced reactions quantityOfOre = binarySearch estimateLow estimateHigh
  where
    estimateLow = quantityOfOre `div` (oreNeededForFuel reactions 1)
    estimateHigh = estimateLow * 2
    binarySearch min max = if min == max || min + 1 == max then min
      else let 
        mid = (min + max) `div` 2
      in
        if oreNeededForFuel reactions mid > quantityOfOre
          then binarySearch min mid
          else binarySearch mid max


testOreNeeded = do
  reactions1 <- load "10 ORE => 10 A\
                      \1 ORE => 1 B \
                      \7 A, 1 B => 1 C \
                      \7 A, 1 C => 1 D \
                      \7 A, 1 D => 1 E \
                      \7 A, 1 E => 1 FUEL"
  oreNeededForFuel reactions1 1 `shouldBe` 31

  reactions2 <- load "9 ORE => 2 A \
                     \8 ORE => 3 B \
                     \7 ORE => 5 C \
                     \3 A, 4 B => 1 AB \
                     \5 B, 7 C => 1 BC \
                     \4 C, 1 A => 1 CA \
                     \2 AB, 3 BC, 4 CA => 1 FUEL"
  oreNeededForFuel reactions2 1 `shouldBe` 165

  reactions3 <- load "157 ORE => 5 NZVS \
                     \ 165 ORE => 6 DCFZ \
                     \ 44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL \
                     \ 12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ \
                     \ 179 ORE => 7 PSHF \
                     \ 177 ORE => 5 HKGWZ \
                     \ 7 DCFZ, 7 PSHF => 2 XJWVT \
                     \ 165 ORE => 2 GPVTF \
                     \ 3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
  oreNeededForFuel reactions3 1 `shouldBe` 13312
  maxFuelProduced reactions3 1000000000000 `shouldBe` 82892753

  reactions4 <- load "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG \
                     \ 17 NVRVD, 3 JNWZP => 8 VPVL \
                     \ 53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL \
                     \ 22 VJHF, 37 MNCFX => 5 FWMGM \
                     \ 139 ORE => 4 NVRVD \
                     \ 144 ORE => 7 JNWZP \
                     \ 5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC \
                     \ 5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV \
                     \ 145 ORE => 6 MNCFX \
                     \ 1 NVRVD => 8 CXFTF \
                     \ 1 VJHF, 6 MNCFX => 4 RFSQX \
                     \ 176 ORE => 6 VJHF"
  oreNeededForFuel reactions4 1 `shouldBe` 180697
  maxFuelProduced reactions4 1000000000000 `shouldBe` 5586022

  reactions5 <- load "171 ORE => 8 CNZTR \
                     \7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL \
                     \114 ORE => 4 BHXH \
                     \14 VRPVC => 6 BMBT \
                     \6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL \
                     \6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT \
                     \15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW \
                     \13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW \
                     \5 BMBT => 4 WPTQ \
                     \189 ORE => 9 KTJDG \
                     \1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP \
                     \12 VRPVC, 27 CNZTR => 2 XDBXC \
                     \15 KTJDG, 12 BHXH => 5 XCVML \
                     \3 BHXH, 2 VRPVC => 7 MZWV \
                     \121 ORE => 7 VRPVC \
                     \7 XCVML => 6 RJRHP \
                     \5 BHXH, 4 VRPVC => 5 LTCX"
  oreNeededForFuel reactions5 1 `shouldBe` 2210736
  maxFuelProduced reactions5 1000000000000 `shouldBe` 460664


-- Main

load :: String -> IO [Reaction]
load text =
  case parse reactions text of
    Ok rs _ ->
      return rs
    Err e a -> do
      putStrLn $ "Expected " ++ e ++ " but found " ++ a
      return []

part1 :: [Reaction] -> IO ()
part1 input =
  putStrLn $ "Part 1 .. " ++ (show $ oreNeededForFuel input 1)


part2 :: [Reaction] -> IO ()
part2 input =
  putStrLn $ "Part 2 .. " ++ (show $ maxFuelProduced input 1000000000000)

main = do
  testParserCombinators
  testModelParser
  testOreNeeded

  input <- readFile "input.txt" >>= load
  part1 input
  part2 input
