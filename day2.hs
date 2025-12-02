{-# LANGUAGE LambdaCase #-}

import Data.List.Split (splitOn, chunksOf)
import Data.List (singleton)


parse :: String -> [(Int, Int)]
parse = map (tup . map read . splitOn "-") . splitOn ","
  where
  tup [a, b] = (a, b)

-- hax
digits :: Int -> [Int]
digits = map (read . singleton) . show

solve1 = sum . filter isInvalid . (>>= uncurry enumFromTo)
  where
  isInvalid id = uncurry (==) $ splitAt (length d `div` 2) $ d
    where
    d = digits id

isInvalid id = or [inv l | l <- [1..length d - 1]]
  where
  d = digits id
  inv len = allEq $ chunksOf len d
  allEq = all (uncurry (==)) . zipNext

zipNext xs = zip xs (drop 1 xs)

solve2 = sum . filter isInvalid . (>>= uncurry enumFromTo)

