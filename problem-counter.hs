-- if problems have commas
--   then split at commas
-- otherwise
--   return the problems (single set)
--                  |
--                  v
-- loop through sets in split problems
-- evaluate # of problems in set
-- increment total # of problems
-- return total

-- NOTES:
-- Use `read` function

--------------------------------------------------

-- Correct output = 18
example = "2, 2, 3-6, 7-11 odd, 12-16 even, 17-27 eoo, 28-38 eoe"

-- | Splits raw text input into rough problem sets at commas
splitAtCommas :: String -> [String]
splitAtCommas x = x

-- | Dissects a given problem set into their individual parts in triples:
--   (start, end, modifier)
dissectSet :: [String] -> [(Int, Int, String)]
dissectSet x = x

-- | Calculate number of problems in a given set and add them all up,
--   returning a total
evaluateSet :: -- I dunno
evaluateSet x = x

main :: IO ()
main = do
  input <- getLine
  putStrLn input

