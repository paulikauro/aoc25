{-# LANGUAGE LambdaCase #-}

import Data.List (scanl')
import Control.Arrow ((&&&))

parse :: String -> [Int]
parse = map parseRot . lines
  where
  parseRot (x:xs) = dir x * read xs
  dir = \case { 'L' -> -1; 'R' -> 1; }

solve1 = count (== 0) . scanl' add 50
  where
  count p = length . filter p

add a b = (a + b) `mod` 100

solve2trash = solve1 . (>>= repl)
  where
  repl x = replicate (abs x) (signum x)

solve2 :: [Int] -> Int
solve2 = fst . foldl' go (0, 50)
  where
  go (counts, acc) x = (counts + count' acc x, add acc x)
  count' acc x = if x < 0 then count (mirror acc) (abs x) else count acc x
  count acc x = wholes + if acc + rest >= 100 then 1 else 0
    where
    (wholes, rest) = x `divMod` 100
  mirror = (`mod` 100) . negate

main = interact $ show . (solve1 &&& solve2) . parse

