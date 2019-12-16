{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace, ord)
import Data.List (dropWhileEnd, intercalate, transpose)
import Test.Hspec
import qualified Data.Text as T

basePattern = [0,1,0,-1]
zeroChar = ord '0'

parseInput :: T.Text -> [Int]
parseInput = map (flip (-) zeroChar . ord) . T.unpack

showOutput :: [Int] -> T.Text
showOutput = T.intercalate "" . map (T.pack . show)

patternForIndex :: Int -> [Int]
patternForIndex n = drop 1 $ concat $ repeat $ concat $ transpose $ replicate n basePattern

pairProduct :: (Int,Int) -> Int
pairProduct (x,y) = x * y

outputRow :: [Int] -> Int -> Int
outputRow input n = (abs $ sum $ map pairProduct $ zip input (patternForIndex n)) `mod` 10

-- extra parameter makes this useful in folds
fftPhase :: [Int] -> Int -> [Int]
fftPhase input _ = map (outputRow input) [1..length input]

runPhases :: Int -> T.Text -> T.Text
runPhases n input = showOutput $ foldl fftPhase (parseInput input) [1..n]

extractMessage :: T.Text -> T.Text
extractMessage input = T.take 8 $ T.drop offset $ fftOutput
  where
    offset = read (T.unpack $ T.take 7 input) :: Int
    realSignal = T.replicate 10000 input
    fftOutput = runPhases 100 realSignal


fftPhaseTests = do
  let phases = scanl fftPhase (parseInput "12345678") [1..4]
  map showOutput phases `shouldBe` ["12345678", "48226158", "34040438", "03415518", "01029498"]

largerTests = do
  T.take 8 (runPhases 100 "80871224585914546619083218645595") `shouldBe` "24176176"
  T.take 8 (runPhases 100 "19617804207202209144916044189917") `shouldBe` "73745418"
  T.take 8 (runPhases 100 "69317163492948606335995924319873") `shouldBe` "52432133"

messageTests = do
  extractMessage "03036732577212944063491565474664" `shouldBe` "84462026"

part1 input =
  let result = T.take 8 $ runPhases 100 input in
  putStrLn $ "Part 1 .. " ++ (show result)

main = do
  fftPhaseTests
  largerTests
  -- messageTests

  input <- fmap (T.strip . T.pack) $ readFile "input.txt"
  part1 input