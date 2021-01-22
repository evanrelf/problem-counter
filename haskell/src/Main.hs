{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (nub)
import Data.Void (Void)
import System.IO (hFlush, stdout)
import Text.Megaparsec (Parsec, eof, parse, sepBy1, some, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, space, space1, string)
import Text.Megaparsec.Error (errorBundlePretty)


data Modifier
  = Even
  | Odd
  | EveryOtherEven
  | EveryOtherOdd
  deriving Show


data Problems
  = Single Int
  | Range Int Int
  | ModifiedRange Int Int Modifier
  deriving Show


type Parser = Parsec Void String


int :: Parser Int
int = read <$> some digitChar


modifier :: Parser Modifier
modifier = asum
  [ string "even" $> Even
  , string "odd" $> Odd
  , string "eoe" $> EveryOtherEven
  , string "eoo" $> EveryOtherOdd
  ]


single :: Parser Problems
single = Single <$> int


range :: Parser Problems
range = do
  x <- int
  _ <- char '-'
  y <- int
  pure (Range x y)


modifiedRange :: Parser Problems
modifiedRange = do
  Range x y <- range
  _ <- space1
  m <- modifier
  pure (ModifiedRange x y m)


problems :: Parser Problems
problems = try modifiedRange <|> try range <|> single


someProblems :: Parser [Problems]
someProblems = problems `sepBy1` (char ',' *> space) <* eof


everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (x : _ : xs) = x : everyOther xs


problemsToList :: Problems -> [Int]
problemsToList = \case
  Single x -> [x]
  Range x y -> [x .. y]
  ModifiedRange x y m ->
    let
      evens = filter even [x .. y]
      odds = filter odd [x .. y]
    in
      case m of
        Even -> evens
        Odd -> odds
        EveryOtherEven -> everyOther evens
        EveryOtherOdd -> everyOther odds


countProblems :: [Problems] -> Int
countProblems = length . nub . concatMap problemsToList


problemCounter :: String -> String
problemCounter input =
  case parse someProblems "" input of
    Left errorBundle -> errorBundlePretty errorBundle
    Right ps -> "Problems: " <> show (countProblems ps)


prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine


main :: IO ()
main = do
  input <- prompt "Enter problems: "
  putStrLn (problemCounter input)
