{-# LANGUAGE NamedFieldPuns #-}

import Data.List       (isInfixOf)
import Data.List.Split (splitOn)
import System.IO       (hFlush, stdout)

type RawProblems = String
type RawSet      = String

data Modifier = All | Even | Odd | Eoe | Eoo | Single deriving (Eq, Show, Read)
data Set = Set { x :: Int      -- ^ Beginning of set
               , y :: Int      -- ^ End of set
               , m :: Modifier -- ^ Set modifier
               } deriving (Eq, Show)

-- | Determines whether a string contains a substring (alias of `isInfixOf`)
contains :: String -> String -> Bool
text `contains` query = query `isInfixOf` text

-- | Splits user-inputted RawProblems into RawSets
splitIntoSets :: RawProblems -> [RawSet]
splitIntoSets problems | problems `contains` ", " = splitOn ", " problems
                       | otherwise                = [problems]

-- | Determines the Modifier described by a string
matchModifier :: String -> Modifier
matchModifier "all"  = All
matchModifier "even" = Even
matchModifier "odd"  = Odd
matchModifier "eoe"  = Eoe
matchModifier "eoo"  = Eoo
matchModifier _      = error "Invalid modifier"

-- | Parses a user-inputted RawSet to create a Set
parse :: RawSet -> Set
parse set
  | not $ set `contains` "-" = Set 0 0 Single
  | otherwise                = Set x y m
    where x = read . head . splitOn "-" $ set
          y | set `contains` " " = read $ splitOn "-" (head $ splitOn " " set) !! 1
            | otherwise          = read $ splitOn "-" set !! 1
          m | set `contains` " " = matchModifier $ splitOn " " set !! 1
            | otherwise          = All

-- | Calculates the number of problems in a Set
evaluate :: Set -> Int
evaluate Set {x, y, m}
  | m == All    = length [x..y]
  | m == Even   = length $ filter even [x..y]
  | m == Odd    = length $ filter odd [x..y]
  | m == Eoe    = if even x
                    then length $ filter even [x,x+4..y]
                    else length $ filter even [x+1,x+5..y]
  | m == Eoo    = if odd x
                    then length $ filter odd [x,x+4..y]
                    else length $ filter odd [x+1,x+5..y]
  | m == Single = 1
  | otherwise   = error "Invalid modifier"

-- | Determines the total number of problems in RawProblems
total :: RawProblems -> Int
total problems = sum $ map (evaluate . parse) $ splitIntoSets problems

-- | `getLine` on the same line as the prompt message
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

