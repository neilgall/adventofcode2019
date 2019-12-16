import Data.Char (isSpace, ord)
import Data.List (dropWhileEnd, intercalate, transpose)
import Test.Hspec

basePattern = [0,1,0,-1]
zeroChar = ord '0'

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

parseInput :: String -> [Int]
parseInput = map (flip (-) zeroChar . ord)

showOutput :: [Int] -> String
showOutput = intercalate "" . map show

patternForIndex :: Int -> [Int]
patternForIndex n = drop 1 $ concat $ repeat $ concat $ transpose $ replicate n basePattern

pairProduct :: (Int,Int) -> Int
pairProduct (x,y) = x * y

outputRow :: [Int] -> Int -> Int
outputRow input n = (abs $ sum $ map pairProduct $ zip input (patternForIndex n)) `mod` 10

-- extra parameter makes this useful in folds
fftPhase :: [Int] -> Int -> [Int]
fftPhase input _ = map (outputRow input) [1..length input]

runPhases :: Int -> String -> String
runPhases n input = showOutput $ foldl fftPhase (parseInput input) [1..n]

extractMessage :: String -> String
extractMessage input = take 8 $ drop offset $ fftOutput
  where
    offset = read (take 7 input) :: Int
    realSignal = concat $ replicate 10000 input
    fftOutput = runPhases 100 realSignal


fftPhaseTests = do
  let phases = scanl fftPhase (parseInput "12345678") [1..4]
  map showOutput phases `shouldBe` ["12345678", "48226158", "34040438", "03415518", "01029498"]

largerTests = do
  runPhases 100 "80871224585914546619083218645595" `shouldStartWith` "24176176"
  runPhases 100 "19617804207202209144916044189917" `shouldStartWith` "73745418"
  runPhases 100 "69317163492948606335995924319873" `shouldStartWith` "52432133"

part1 input =
  let result = take 8 $ runPhases 100 input in
  putStrLn $ "Part 1 .. " ++ result

main = do
  fftPhaseTests
  largerTests

  input <- fmap strip $ readFile "input.txt"
  part1 input