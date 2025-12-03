{-# LANGUAGE LambdaCase #-}

import Data.List (singleton, init, elemIndex, splitAt)
import Data.Array (Array)
import Data.Array qualified as A
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

parse :: String -> [[Int]]
parse = map (map (read . singleton)) . lines


solve1 :: [[Int]] -> Int
solve1 = sum . map maxJolt
  where
  maxJolt xs = 10 * first + second
    where
    first = maximum $ init xs
    second = maximum $ tail $ dropWhile (/= first) xs

solve2' :: [[Int]] -> Int
solve2' = sum . map (step 12 0)
  where
  step 0 bs xs = bs
  step n bs xs = step (n - 1) (10 * bs + b) rest
    where
    b = maximum (dropLast (n - 1) xs)
    i = fromJust $ elemIndex b xs
    (_, rest) = splitAt (i + 1) xs

-- bad, but works
dropLast n xs = take (length xs - n) xs

solve2 :: [[Int]] -> Int
solve2 bats = sum $ map calcMaxJolt bats
  where
  lineLen = length $ head bats
  dpBounds = ((1, 1), (12, lineLen))
  calcMaxJolt xs = let f = arrayMemo dpBounds (maxJolt (A.listArray (1, lineLen) xs))
    in maximum [f (12, i) | i <- [1..lineLen]]
  -- maxJolt xs r (n, i) = maximum joltage with n batteries
  -- chosen from the first i batteries in xs
  maxJolt xs r = \case
    -- 1 battery -> ith battery only one that's included
    (1, i) -> xs A.! i
    (n, i)
      -- enough batteries to choose from
      | i >= n -> 10 * maximum [r (n - 1, j) | j <- [1..i - 1]] + xs A.! i
      -- not enough
      | otherwise -> 0

arrayMemo bounds f = lookup
  where
  table = A.array bounds $ map (\i -> (i, f lookup i)) $ A.range bounds
  lookup = (table A.!)

