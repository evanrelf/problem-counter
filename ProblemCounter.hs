{-# LANGUAGE NamedFieldPuns #-}

import Data.Char (toUpper)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import System.IO (hFlush, stdout)

type RawProblems = String

type RawSet = String

type RawModifier = String

data Modifier
  = All
  | Even
  | Odd
  | Eoe -- ^ Every other even
  | Eoo -- ^ Every other odd
  | Single
  deriving (Eq, Show, Read)

data Set = Set
  { x :: Int -- ^ Beginning of set
  , y :: Int -- ^ End of set
  , m :: Modifier -- ^ Set modifier
  } deriving (Eq, Show)

-- | Alias of `isInfixOf`
contains :: Eq a => [a] -> [a] -> Bool
text `contains` query = query `isInfixOf` text

-- | Removes duplicate elements from a list
-- [1, 1, 2, 3, 4, 1, 1, 5, 6, 3]
dedupe :: Eq a => [a] -> [a]
dedupe [] = []
dedupe (x:xs) = x : dedupe (filter (/= x) xs)

-- | Splits user-inputted RawProblems into RawSets
splitIntoSets :: RawProblems -> [RawSet]
splitIntoSets problems
  | problems `contains` ", " = splitOn ", " problems
  | otherwise = [problems]

-- | Determines the Modifier described by a string
matchModifier :: RawModifier -> Modifier
matchModifier "" = error "Zero-length modifier"
matchModifier (x:xs) = read $ toUpper x : xs

-- | Parses a user-inputted RawSet to create a Set
parse :: RawSet -> Set
parse set
  | '-' `notElem` set = Set 0 0 Single
  | otherwise = Set x y m
  where
    x = (read . head . splitOn "-") set
    y
      | ' ' `elem` set = read $ splitOn "-" (head $ words set) !! 1
      | otherwise = read $ splitOn "-" set !! 1
    m
      | ' ' `elem` set = matchModifier $ words set !! 1
      | otherwise = All

-- | Calculates which problem numbers are in a set
evaluate :: Set -> [Int]
evaluate Set {x, y, m} =
  case m of
    All -> [x .. y]
    Even -> filter even [x .. y]
    Odd -> filter odd [x .. y]
    Eoe ->
      if even x
        then filter even [x,x + 4 .. y]
        else filter even [x + 1,x + 5 .. y]
    Eoo ->
      if odd x
        then filter odd [x,x + 4 .. y]
        else filter odd [x + 1,x + 5 .. y]
    Single -> [1]

-- | Determines the total number of problems in RawProblems
total :: RawProblems -> Int
total problems =
  length $ (dedupe . concat) $ map (evaluate . parse) (splitIntoSets problems)

-- | `getLine` on the same line as the prompt message
-- Credit: https://stackoverflow.com/a/13190872/1664444
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

main :: IO ()
main = do
  input <- prompt "Input problems: "
  let result = show $ total input
  putStrLn $ "Total problems: " ++ result ++ "\n"
