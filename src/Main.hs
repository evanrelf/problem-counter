module Main where

import Data.Char (toLower, toUpper)
import Data.List (dropWhileEnd)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Regex (mkRegex, splitRegex, subRegex)

type InputString = String

type SetString = String

data Modifier
  = All
  | Even
  | Odd
  | Eoe
  | Eoo
  | Single
  deriving (Show, Read)

data Set =
  Set (Int, Int)
      Modifier
  deriving (Show)

dedupe :: (Eq a) => [a] -> [a]
dedupe [] = []
dedupe (x:xs) = x : dedupe (filter (/= x) xs)

takeFirstWhile :: (a -> Bool) -> [a] -> [a]
takeFirstWhile b = takeWhile b . dropWhile (not . b)

splitIntoSets :: InputString -> [SetString]
splitIntoSets = filter (/= "") . splitRegex (mkRegex ",")

readSet :: SetString -> Set
readSet set
  | '-' `notElem` set = Set (x, x) Single
  | ' ' `notElem` set = Set (x, y) All
  | otherwise = Set (x, y) m
  where
    x = (read . takeFirstWhile (`elem` ['0' .. '9'])) set
    y = (read . tail . dropWhile (/= '-') . head . words) set
    m = (read . (\(c:cs) -> toUpper c : cs) . last . words) set

evaluateSet :: Set -> [Int]
evaluateSet (Set (x, y) m) =
  case m of
    All -> [x .. y]
    Even -> evens
    Odd -> odds
    Eoe -> everyOther evens
    Eoo -> everyOther evens
    Single -> [x]
  where
    evens = filter even [x .. y]
    odds = filter odd [x .. y]
    everyOther xs = [xs !! i | i <- [0,2 .. length xs - 1]]

count :: InputString -> Int
count =
  length . dedupe . concatMap (evaluateSet . readSet) . splitIntoSets

helpText :: String
helpText =
  "EXAMPLE INPUT:\n\
  \    - '1, 2, 3' => 3\n\
  \    - '1-20, 21-31 odd' => 26\n\
  \    - '5, 6-22 even, 30-60 eoe, 51, 52' => 20\n\
  \\n\
  \PROBLEM SET MODIFIERS:\n\
  \    - all (default when no modifier specified)\n\
  \    - even\n\
  \    - odd\n\
  \    - eoe (every other even)\n\
  \    - eoo (every other odd)\n"

problemCounter :: IO ()
problemCounter = do
  putStr "> "
  hFlush stdout
  input <- getLine
  if head input == ':'
    then case tail input of
           "q" -> exitSuccess
           "h" -> putStrLn helpText
           _ -> putStrLn "ERROR: Invalid command\n"
    else let result = count input
         in putStrLn $ "Total problems: " ++ show result ++ "\n"
  problemCounter

main :: IO ()
main = do
  putStrLn "Type :h for help, :q to quit"
  problemCounter
