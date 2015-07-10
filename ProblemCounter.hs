module Main where

import Data.List       (isInfixOf)
import Data.List.Split (splitOn)
import System.IO       (hFlush, stdout)

-- TODO: Use a Set datatype instead of a type
type RawProblems = String
type RawSet      = String
type Set         = (Int, Int, String)

-- | Simplifies `isInfixOf` into a more readable-English version
contains :: String -> String -> Bool
text `contains` query = query `isInfixOf` text

-- | Splits RawProblems into a list of RawProblemSets at any commas
splitIntoSets :: RawProblems -> [RawSet]
splitIntoSets problems | problems `contains` ", " = splitOn ", " problems
                       | otherwise                = [problems]

-- | Splits a RawSet into a Set
dissectSet :: RawSet -> Set
dissectSet set
  | not $ set `contains` "-" = (0, 0, "single")
  | otherwise                = (read x, read y, m)
    where x = head $ splitOn "-" set
          y | set `contains` " " = splitOn "-" (head $ splitOn " " set) !! 1
            | otherwise          = splitOn "-" set !! 1
          m | set `contains` " " = splitOn " " set !! 1
            | otherwise          = "all"

-- | Verifies that a Set is valid according to a list of conditions
verifySet :: Set -> Bool
verifySet (_, _, "single") = True
verifySet (x, y, m) =
  all (==True) [ m `elem` ["all", "even", "odd", "eoe", "eoo", "single"]
               , x > 0
               , y > x ]

-- | Determines the number of problems in a Set
evaluateSet :: Set -> Int
evaluateSet (x, y, m)
  | m == "all"                = (y - x) + 1
  | m == "even" || m == "odd" = ((correctY 2 - x) `div` 2) + 1
  | m == "eoe"  || m == "eoo" = ((correctY 4 - x) `div` 4) + 1
  | m == "single"             = 1
  | otherwise                 = 0  -- If somehow `verifyProblems` fails...
    -- Corrects for invalid y value with "eoe" and "eoo" modifiers. For
    -- example, "34-64 eoe" is not valid, because the set ends at 62.
    where correctY offset = y - ((y - x) `mod` offset)

-- | Checks every Set with `verifySet` to verify the entire input
verifyProblems :: RawProblems -> Bool
verifyProblems problems =
  all (==True) $ map (verifySet . dissectSet) (splitIntoSets problems)

-- | Orchestratess all the above functions to produce a total count
countProblems :: RawProblems -> Int
countProblems problems = sum $ map processSet sets
  where processSet = evaluateSet . dissectSet
        sets       = splitIntoSets problems

-- | Prompts for and accepts user input on the same line
-- Taken from http://stackoverflow.com/a/13190872
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- | Interacts with the user (duh)
main :: IO ()
main = do
  input <- prompt "Input problems: "
  if verifyProblems input
    then do
      putStrLn $ "Total problems: " ++ show (countProblems input) ++ "\n"
      main -- Loop indefinitely
    else error "Invalid input"
