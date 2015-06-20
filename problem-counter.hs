import Control.Monad   (forever)
import Data.List       (isInfixOf)
import Data.List.Split (splitOn)

contains :: String -> String -> Bool
contains text query = query `isInfixOf` text

splitIntoSets :: String -> [String]
splitIntoSets problems = if problems `contains` ", "
                           then splitOn ", " problems
                           else [problems]

dissectSet :: String -> (Int, Int, String)
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

verifySet :: (Int, Int, String) -> Bool
verifySet (0, 0, "single") = True
verifySet (x, y, m) =
  all (==True) [ m `elem` ["all", "even", "odd", "eoe", "eoo", "single"]
               , x > 0
               , y > x
               ]

evaluateSet :: (Int, Int, String) -> Int
evaluateSet (x, y, m)
  | m == "all" && (y - x) > 999 = (y - x) + 1  -- because math is faster
  | m == "all"                  = length [x..y]
  | m == "even" || m == "odd"   = length [x,x+2..correctY 2]
  | m == "eoe"  || m == "eoo"   = length [x,x+4..correctY 4]
  | m == "single"               = 1
  | otherwise                   = 0 -- TODO: Throw error
    -- Corrects for invalid y value in eoe and eoo. For example,
    -- "32-64 eoe" is not valid, because the set ends at 62.
    where correctY offset = y - ((y - x) `mod` offset)

verifyProblems :: String -> Bool
verifyProblems problems =
  all (==True) $ map (verifySet . dissectSet) (splitIntoSets problems)

countProblems :: String -> Int
countProblems problems = sum $ map (evaluateSet . dissectSet) (splitIntoSets problems)

main :: IO ()
main = forever $ do
  putStrLn "Input problems: "
  input <- getLine
  if verifyProblems input
    then putStrLn $ "Total problems: " ++ show (countProblems input) ++ "\n"
    else error "Invalid input"
