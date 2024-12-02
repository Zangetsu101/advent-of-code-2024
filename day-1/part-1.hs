module Main where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (sort)

partOne = show . sum . map (abs . uncurry subtract) . uncurry zip . bimap sort sort . unzip . fmap ((\[x, y] -> (x, y)) . fmap read . words) . lines

main :: IO ()
main = interact partOne
