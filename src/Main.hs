module Main where

import Data.List (nub)
import System.IO (hFlush, stdout)
import Text.Megaparsec (Parsec, optional, parse, some, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, space, string)

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

readModifier :: String -> Modifier
readModifier s =
  case s of
    "even" -> Even
    "odd" -> Odd
    "eoe" -> EveryOtherEven
    "eoo" -> EveryOtherOdd
    _ -> error $ "Invalid modifier '" ++ s ++ "'"

modifier :: Parser Modifier
modifier = do
  m <- string "even"
   <|> string "odd"
   <|> string "eoe"
   <|> string "eoo"
  return $ readModifier m

single :: Parser Problems
single = do
  x <- some digitChar
  return $ Single (read x)

range :: Parser Problems
range = do
  Single x <- single
  _ <- char '-'
  y <- some digitChar
  return $ Range x (read y)

modifiedRange :: Parser Problems
modifiedRange = do
  Range x y <- range
  _ <- space
  ModifiedRange x y <$> modifier

problems :: Parser Problems
problems = try modifiedRange <|> try range <|> single

someProblems :: Parser [Problems]
someProblems = some $ do
    space
    p <- problems
    _ <- optional $ char ','
    return p

problemsToList :: Problems -> [Int]
problemsToList (Single x) = [x]
problemsToList (Range x y) = [x .. y]
problemsToList (ModifiedRange x y m) =
  case m of
    Even -> filter even [x .. y]
    Odd -> filter odd [x .. y]
    EveryOtherEven -> everyOther $ filter even [x .. y]
    EveryOtherOdd -> everyOther $ filter odd [x .. y]
  where everyOther xs = [xs !! i | i <- [0,2 .. length xs - 1]]

problemCounter :: String -> String
problemCounter s = either leftFn rightFn (parse someProblems "" s)
  where leftFn x = "Error: Bad input\n\n" ++ show x
        rightFn x =
          "Problems: " ++ (show . length . nub . concatMap problemsToList $ x)

main :: IO ()
main = prompt "Enter problems: " >>= putStrLn . problemCounter
  where prompt s = putStr s >> hFlush stdout >> getLine
