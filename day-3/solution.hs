module Main where

import Data.Char (isNumber)

getFirstNum :: String -> Maybe (Int, String)
getFirstNum ('m' : 'u' : 'l' : '(' : rest) = case span isNumber rest of
  ([], _) -> Nothing
  (num, rest) -> Just (read num, rest)
getFirstNum _ = Nothing

getSecondNum :: String -> Maybe (Int, String)
getSecondNum (',' : rest) = case span isNumber rest of
  ([], _) -> Nothing
  (num, ')' : rest) -> Just (read num, rest)
  (_, _) -> Nothing
getSecondNum _ = Nothing

getMulitplication :: String -> Maybe((Int, Int), String)
getMulitplication str = do
  (firstNum, rest) <- getFirstNum str
  (secondNum, rest) <- getSecondNum rest
  return ((firstNum, secondNum), rest)

parseInput str@(_ : xs) = case getMulitplication str of
  Just(numPair, rest) -> numPair : parseInput rest
  Nothing -> parseInput xs
parseInput [] = []

partOne = show . sum . map (uncurry (*)) . parseInput

main :: IO ()
main = interact partOne
