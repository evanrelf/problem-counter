module Main (main) where

import Control.Applicative ((<*))
import Data.List (nub)
import Data.Void (Void)
import System.IO (hFlush, stdout)
import Text.Megaparsec (Parsec, eof, parse, sepBy1, some, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, space, space1, string)
import Text.Megaparsec.Error (parseErrorPretty')

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

type Parser = Parsec Void String

int :: Parser Int
int = read <$> some digitChar

modifier :: Parser Modifier
modifier =
  let f s v = string s >> return v
   in f "even" Even
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
someProblems = problems `sepBy1` (char ',' >> space) <* eof

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
problemCounter s =
  let count = length . nub . concatMap problemsToList
      rightFn ps = "Problems: " <> show (count ps)
  in either (parseErrorPretty' s) rightFn (parse someProblems "" s)

main :: IO ()
main =
  let prompt s = putStr s >> hFlush stdout >> getLine
  in prompt "Enter problems: " >>= putStrLn . problemCounter
