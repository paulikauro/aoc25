{-# LANGUAGE LambdaCase #-}

import Data.List (scanl')

parse :: String -> [Int]
parse = map parseRot . lines
  where
  parseRot (x:xs) = dir x * read xs
  dir = \case { 'L' -> -1; 'R' -> 1; }

solve1 = show . count (== 0) . scanl' add 50
  where
  count p = length . filter p
  add a b = (a + b + 100) `mod` 100 -- vtf

main = interact $ solve1 . parse

