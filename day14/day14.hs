import Data.Char (isAsciiUpper, isDigit, isSpace)
import Data.List (intersperse, stripPrefix)
import Prelude hiding (sequence)
import Test.Hspec hiding (before)

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

parse :: Parser input value -> input -> ParseResult input value
parse (Parser p) input = p input

sequence :: (p -> q -> r) -> Parser i p -> Parser i q -> Parser i r
sequence c (Parser p) (Parser q) = Parser seq
  where
    seq input = case p input of
      Ok r rest -> fmap (\s -> c r s) (q rest)
      Err e a -> Err e a

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
x `before` p = sequence (\_ v -> v) x p

followedBy :: Parser i a -> Parser i x -> Parser i a
p `followedBy` x = sequence (\v _ -> v) p x

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

data Material = Material Int String
  deriving (Eq, Show)

data Reaction = Reaction [Material] Material
  deriving (Eq)

instance Show Reaction where
  show (Reaction inputs output) = inputs' ++ " => " ++ output'
    where
      inputs' = concat $ intersperse "," (map show inputs)
      output' = show output


-- Input parser

quantity :: Parser String Material
quantity = sequence Material (integer `followedBy` whitespace) chemical

reaction :: Parser String Reaction
reaction = sequence Reaction inputs outputs
  where
    inputs = quantity `sepBy` literal ", "
    outputs = literal " => " `before` quantity

reactions :: Parser String [Reaction]
reactions = reaction `sepBy` whitespace


testModelParser = do
  parse quantity "54 FUEL" `shouldBe` Ok (Material 54 "FUEL") ""
  parse reaction "8 A, 1 B => 1 C" `shouldBe` Ok (Reaction [Material 8 "A", Material 1 "B"] (Material 1 "C")) ""

main = do
  testParserCombinators
  testModelParser

  input <- fmap (parse reactions) $ readFile "input.txt"
  putStrLn (show input)
