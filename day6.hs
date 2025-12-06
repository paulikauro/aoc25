{-# LANGUAGE LambdaCase #-}

import Data.List (transpose, unsnoc)
import Data.List.Split (splitWhen)
import Data.Char (isSpace)

parse1 :: String -> [([Int], String)]
parse1 = map line . transpose . map words . lines
  where
  line xs = let Just (xs', op) = unsnoc xs in (map read xs', op)


solve1 :: [([Int], String)] -> Int
solve1 = sum . map calc
  where
  calc (xs, op) = foldl1 (readOp op) xs

readOp = \case { "*" -> (*); "+" -> (+) }

solve2 = sum . map calc . splitWhen (all isSpace) . transpose . lines
  where
  calc (x:xs) = foldl1 op $ map (read . clean) (x:xs)
    where
    op = readOp [(last x)]
    clean = filter (not . \x -> isSpace x || x == '+' || x == '*')
