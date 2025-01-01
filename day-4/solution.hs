module Main where

import Data.Maybe (mapMaybe)
import Control.Applicative (Applicative(liftA2))

(!?) :: [a] -> Int -> Maybe a
{-# INLINEABLE (!?) #-}
xs !? n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n

parseInput = lines

add (x, y) (u, v) = (x + u, y + v)

countXmas grid row col = length $ filter ("XMAS" ==) allWords
 where
  indices = (fmap . fmap) (add (row, col)) directions
  allWords = fmap (mapMaybe (\(r, c) -> grid !? r >>= (!? c))) indices
  right = [(0, 0), (0, 1), (0, 2), (0, 3)]
  left = [(0, 0), (0, -1), (0, -2), (0, -3)]
  up = [(0, 0), (-1, 0), (-2, 0), (-3, 0)]
  down = [(0, 0), (1, 0), (2, 0), (3, 0)]
  upRight = [(0, 0), (-1, 1), (-2, 2), (-3, 3)]
  upLeft = [(0, 0), (-1, -1), (-2, -2), (-3, -3)]
  downRight = [(0, 0), (1, 1), (2, 2), (3, 3)]
  downLeft = [(0, 0), (1, -1), (2, -2), (3, -3)]
  directions = [right, left, up, down, upRight, upLeft, downRight, downLeft] :: [[(Int, Int)]]

partOne input = sum . fmap (uncurry (countXmas grid)) $ allCells
 where
  grid = parseInput input
  rows = length grid
  cols = length $ head grid
  allCells = [(r, c) | r <- [0..rows], c <- [0..cols]]

isCrossMas grid row col = all (liftA2 (||) ("MAS" ==) ("SAM" ==)) words
  where
    diagonals = [[(-1, -1), (0, 0), (1, 1)], [(-1, 1), (0, 0), (1, -1)]]
    indices = (fmap . fmap) (add (row, col)) diagonals
    words = fmap (mapMaybe (\(r, c) -> grid !? r >>= (!? c))) indices

partTwo input = sum . fmap (fromEnum . uncurry (isCrossMas grid)) $ allCells
 where
  grid = parseInput input
  rows = length grid
  cols = length $ head grid
  allCells = [(r, c) | r <- [0..rows], c <- [0..cols]]

main :: IO ()
main = print . partTwo =<< getContents
