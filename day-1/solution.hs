module Main where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (sort)

parseLists :: String -> ([Int], [Int])
parseLists = unzip . fmap ((\[x, y] -> (x, y)) . fmap read . words) . lines

partOne = show . sum . map (abs . uncurry subtract) . uncurry zip . bimap sort sort . parseLists

partTwo = show . sum . (\(a, b) -> map (\x -> x * length (filter (== x) b)) a) . parseLists

main :: IO ()
main = interact partTwo
