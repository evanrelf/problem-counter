module Main (main) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (concatMap, filter, fromFoldable, length, nub, some, (..), (:))
import Data.Either (either)
import Data.Foldable (foldl)
import Data.Identity (Identity)
import Data.Int (even, fromString, odd)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested (Tuple3, tuple3, uncurry3)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.ReadLine as RL
import Text.Parsing.Parser (ParserT, runParser)
import Text.Parsing.Parser.Combinators (sepBy1, try)
import Text.Parsing.Parser.String (char, eof, string, satisfy, whiteSpace)
import Text.Parsing.Parser.Token (digit)

type Parser = ParserT String Identity

data Modifier
  = Even
  | Odd
  | EveryOtherEven
  | EveryOtherOdd

data Problems
  = Single Int
  | Range Int Int
  | ModifiedRange Int Int Modifier

int :: Parser Int
int = (fromMaybe 0 <<< fromString <<< fromCharArray) <$> some digit

modifier :: Parser Modifier
modifier =
  let f s v = string s *> pure v
   in f "even" Even
  <|> f "odd" Odd
  <|> f "eoe" EveryOtherEven
  <|> f "eoo" EveryOtherOdd

range :: Parser (Tuple Int Int)
range = do
  x <- int
  _ <- char '-'
  y <- int
  pure $ Tuple x y

whiteSpace' :: Parser String
whiteSpace' = do
  cs <- some $ satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
  pure $ fromCharArray cs

modifiedRange :: Parser (Tuple3 Int Int Modifier)
modifiedRange = do
  Tuple x y <- range
  _ <- whiteSpace'
  m <- modifier
  pure $ tuple3 x y m

problems :: Parser Problems
problems = try (uncurry3 ModifiedRange <$> modifiedRange)
       <|> try (uncurry Range <$> range)
       <|> Single <$> int

listOfProblems :: Parser (Array Problems)
listOfProblems = fromFoldable <$> problems `sepBy1` (char ',' *> whiteSpace) <* eof

everyOther :: ∀ a. Array a -> Array a
everyOther = snd <<< foldl (\(Tuple t xs) x -> Tuple (not t) (if t then x : xs else xs)) (Tuple true [])

problemsToList :: Problems -> Array Int
problemsToList (Single x) = [x]
problemsToList (Range x y) = x .. y
problemsToList (ModifiedRange x y m) =
  let evens = filter even $ x .. y
      odds = filter odd $ x .. y
  in case m of
       Even -> evens
       Odd -> odds
       EveryOtherEven -> everyOther evens
       EveryOtherOdd -> everyOther odds

problemCounter :: String -> String
problemCounter s =
  let count = length <<< nub <<< concatMap problemsToList
      rightFn ps = "Problems: " <> show (count ps)
  in either show rightFn (runParser s listOfProblems)

prompt :: String -> Aff String
prompt msg = do
  answer <- AVar.empty
  console <- liftEffect $ RL.createConsoleInterface RL.noCompletion
  liftEffect $ RL.question msg (\line -> Aff.launchAff_ $ do
    liftEffect $ RL.close console
    AVar.put line answer) console
  AVar.take answer

main :: Effect Unit
main = Aff.launchAff_ $ do
  input <- prompt "Enter problems: "
  liftEffect $ log $ problemCounter input
