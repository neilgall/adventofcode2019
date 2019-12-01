import Test.Hspec

load :: String -> [Int]
load = map read . lines

moduleFuel :: Int -> Int
moduleFuel mass = (mass `div` 3) - 2

fuelFuel :: Int -> Int
fuelFuel fuel
  | moduleFuel fuel <= 0 = fuel
  | otherwise = fuel + fuelFuel (moduleFuel fuel)

part1 :: [Int] -> Int
part1 = sum . map moduleFuel

part2 :: [Int] -> Int
part2 = sum . map (fuelFuel . moduleFuel)

testModuleFuel = do
  moduleFuel 12 `shouldBe` 2
  moduleFuel 14 `shouldBe` 2
  moduleFuel 1969 `shouldBe` 654
  moduleFuel 100756 `shouldBe` 33583

testFuelFuel = do
  fuelFuel 2 `shouldBe` 2
  fuelFuel 654 `shouldBe` 966
  fuelFuel 33583 `shouldBe` 50346

test = do
  testModuleFuel
  testFuelFuel

main = do
  test
  input <- fmap load $ readFile "input.txt"
  putStrLn $ "Part 1 : " ++ (show $ part1 input)
  putStrLn $ "Part 2 : " ++ (show $ part2 input)
