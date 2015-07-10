{-# LANGUAGE NamedFieldPuns #-}

import Data.List       (isInfixOf)
import Data.List.Split (splitOn)
import System.IO       (hFlush, stdout)

type RawProblems = String
type RawSet      = String

data Modifier = All | Even | Odd | Eoe | Eoo | Single deriving (Eq, Show, Read)
data Set      = Set { x :: Int       -- ^ Beginning of set
                    , y :: Int       -- ^ End of set
                    , m :: Modifier  -- ^ Set modifier
                    } deriving (Eq, Show)

-- | Determines whether a string contains a substring (alias of `isInfixOf`)
contains :: String -> String -> Bool
text `contains` query = query `isInfixOf` text

-- | Splits user-inputted RawProblems into RawSets
splitIntoSets :: RawProblems -> [RawSet]
splitIntoSets problems | problems `contains` ", " = splitOn ", " problems
                       | otherwise             = [problems]

-- | Determines the Modifier described by a string
parseModifier :: String -> Modifier
parseModifier m | m == "all"  = All
                | m == "even" = Even
                | m == "odd"  = Odd
                | m == "eoe"  = Eoe
                | m == "eoo"  = Eoo
                | otherwise   = error "Invalid modifier"

-- | Parses a user-inputted RawSet to create a Set
dissect :: RawSet -> Set
dissect set
  | not $ set `contains` "-" = (Set 0 0 Single)
  | otherwise                = (Set x y m)
    where x = read . head . splitOn "-" $ set
          y | set `contains` " " = read $ splitOn "-" (head $ splitOn " " set) !! 1
            | otherwise          = read $ splitOn "-" set !! 1
          m | set `contains` " " = parseModifier $ splitOn " " set !! 1
            | otherwise          = All

-- | Calculates the number of problems in a Set
evaluate :: Set -> Int
evaluate Set {x, y, m}
  | m == All              = (y - x) + 1
  | m == Even || m == Odd = ((y' 2 - x) `div` 2) + 1
  | m == Eoe  || m == Eoo = ((y' 4 - x) `div` 4) + 1
  | m == Single           = 1
  | otherwise             = 0
    -- Corrects for invalid y value with Eoe and Eoo
    -- (e.g. "34-64 eoe" is invalid because the set ends at 62)
    where y' offset = y - ((y - x) `mod` offset)

-- | Determines the total number of problems in RawProblems
total :: RawProblems -> Int
total problems = sum $ map (evaluate . dissect) $ splitIntoSets problems

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
