module Main where

import Control.Applicative (Applicative (liftA2))

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

safeList :: (Ord a, Num a) => [a] -> Bool
safeList = liftA2 (||) (and . (zipWith safePairInc <*> tail)) (and . (zipWith safePairDec <*> tail))
 where
  safePairInc a b = a < b && b - a <= 3
  safePairDec a b = a > b && a - b <= 3

partOne = show . sum . map (fromEnum . safeList) . parseInput

dampenedLists :: [a] -> [[a]]
dampenedLists xs = map (`dampenList` xs) [0 .. length xs]
 where
  dampenList i = map snd . filter ((/= i) . fst) . zip [0 ..]

safeList' :: (Ord a, Num a) => [a] -> Bool
safeList' = any safeList . dampenedLists

partTwo = show . sum . map (fromEnum . safeList') . parseInput

main :: IO ()
main = interact partTwo
