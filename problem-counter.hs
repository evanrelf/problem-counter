import Data.List       (isInfixOf)
import Data.List.Split (splitOn)

type RawProblems   = String
type RawProblemSet = String
type ProblemSet    = (Int, Int, String)
type Total         = Int

-- | Simplifies `isInfixOf` into a more readable-English version
contains :: String -> String -> Bool
text `contains` query = query `isInfixOf` text

-- | Splits RawProblems into a list of RawProblemSets at any commas
splitIntoSets :: RawProblems -> [RawProblemSet]
splitIntoSets problems = if problems `contains` ", "
                           then splitOn ", " problems
                           else [problems]

-- | Splits a RawProblemSet into a ProblemSet
dissectSet :: RawProblemSet -> ProblemSet
dissectSet set
  | not $ set `contains` "-" = (0, 0, "single")
  | otherwise = (read x, read y, m)
    where x = head $ splitOn "-" set
          y = if set `contains` " "
                then splitOn "-" (head $ splitOn " " set) !! 1
                else splitOn "-" set !! 1
          m = if set `contains` " "
                then splitOn " " set !! 1
                else "all"

-- | Verifies that a ProblemSet is valid according to a list of conditions
verifySet :: ProblemSet -> Bool
verifySet (_, _, "single") = True
verifySet (x, y, m) =
  all (==True) [ m `elem` ["all", "even", "odd", "eoe", "eoo", "single"]
               , x > 0
               , y > x
               ]

-- | Determines the number of problems in a ProblemSet
evaluateSet :: ProblemSet -> Total
evaluateSet (x, y, m)
  | m == "all"                  = (y - x) + 1
  | m == "even" || m == "odd"   = ((correctY 2 - x) `div` 2) + 1
  | m == "eoe"  || m == "eoo"   = ((correctY 4 - x) `div` 4) + 1
  | m == "single"               = 1
  | otherwise                   = 0  -- If somehow `verifyProblems` fails
    -- Corrects for invalid y value with "eoe" and "eoo" modifiers. For
    -- example, "34-64 eoe" is not valid, because the set ends at 62.
    where correctY offset = y - ((y - x) `mod` offset)

-- | Checks each ProblemSet with `verifySet` to verify the entire input
verifyProblems :: RawProblems -> Bool
verifyProblems problems =
  all (==True) $ map (verifySet . dissectSet) (splitIntoSets problems)

-- | Orchestratess all the above functions to produce a total count
countProblems :: RawProblems -> Total
countProblems problems = sum $ map processSet sets
  where processSet = evaluateSet . dissectSet
        sets       = splitIntoSets problems

-- | Interacts with the user (duh)
main :: IO ()
main = do
  putStrLn "Input problems: "
  input <- getLine
  if verifyProblems input
    then do
      putStrLn $ "Total problems: " ++ show (countProblems input) ++ "\n"
      main
    else error "Invalid input"
