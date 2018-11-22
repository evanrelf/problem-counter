module Main where

import Control.Applicative ((<*))
import Data.List (nub)
import System.IO (hFlush, stdout)
import Text.Megaparsec (Parsec, eof, parse, sepBy, some, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, space, space1, string)

data Modifier
  = Even
  | Odd
  | EveryOtherEven
  | EveryOtherOdd
  deriving (Show)

data Problems
  = Single Int
  | Range Int Int
  | ModifiedRange Int Int Modifier
  deriving (Show)

type Parser = Parsec () String

int :: Parser Int
int = read <$> some digitChar

modifier :: Parser Modifier
modifier = let f s v = string s >> return v in
      f "even" Even
  <|> f "odd" Odd
  <|> f "eoe" EveryOtherEven
  <|> f "eoo" EveryOtherOdd

single :: Parser Problems
single = Single <$> int

range :: Parser Problems
range = do
  Single x <- single
  _ <- char '-'
  Range x <$> int

modifiedRange :: Parser Problems
modifiedRange = do
  Range x y <- range
  _ <- space1
  ModifiedRange x y <$> modifier

problems :: Parser Problems
problems = try modifiedRange <|> try range <|> single

someProblems :: Parser [Problems]
someProblems = problems `sepBy` (char ',' >> space) <* eof

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (x:_:xs) = x : everyOther xs

problemsToList :: Problems -> [Int]
problemsToList (Single x) = [x]
problemsToList (Range x y) = [x .. y]
problemsToList (ModifiedRange x y m) =
  let evens = filter even [x .. y]
      odds = filter odd [x .. y]
  in case m of
       Even -> evens
       Odd -> odds
       EveryOtherEven -> everyOther evens
       EveryOtherOdd -> everyOther odds

problemCounter :: String -> String
problemCounter s = either leftFn rightFn (parse someProblems "" s)
  where leftFn x = "Error: Bad input\n\n" <> show x
        rightFn x =
          "Problems: " <> (show . length . nub . concatMap problemsToList $ x)

main :: IO ()
main = prompt "Enter problems: " >>= putStrLn . problemCounter
  where prompt s = putStr s >> hFlush stdout >> getLine
