module Main where

import Data.Char (toUpper)
import Data.List (nub)
import Text.Megaparsec (Parsec, optional, parse, some, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, space, string)

data Modifier
  = All
  | Even
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

mkModifier :: String -> Modifier
mkModifier s =
  case s of
    "all" -> All
    "even" -> Even
    "odd" -> Odd
    "eoe" -> EveryOtherEven
    "eoo" -> EveryOtherOdd
    _ -> error $ "Invalid modifier '" ++ s ++ "'"

modifier :: Parser Modifier
modifier = do
  m <- string "all"
   <|> string "even"
   <|> string "odd"
   <|> string "eoe"
   <|> string "eoo"
  return $ mkModifier m

single :: Parser Problems
single = do
  x <- some digitChar
  return $ Single (read x)

range :: Parser Problems
range = do
  x <- some digitChar
  _ <- char '-'
  y <- some digitChar
  return $ Range (read x) (read y)

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
    All -> [x .. y]
    Even -> filter even [x .. y]
    Odd -> filter odd [x .. y]
    EveryOtherEven -> everyOther $ filter even [x .. y]
    EveryOtherOdd -> everyOther $ filter odd [x .. y]
  where everyOther xs = [xs !! i | i <- [0,2 .. length xs - 1]]

problemCounter :: String -> String
problemCounter s = either leftFn rightFn (parse someProblems "" s)
  where leftFn x = "Error: " ++ show x
        rightFn x =
          "Problems: " ++ (show . length . nub . concatMap problemsToList $ x)

main :: IO ()
main = putStr "Enter problems: " >> getLine >>= putStrLn . problemCounter
