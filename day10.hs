{-# LANGUAGE BlockArguments #-}
import Data.Bits (shiftL, (.|.), xor, bit)
import Data.List.Split (splitOn)
import Data.List (uncons, unsnoc, intercalate)
import Data.Foldable (toList)
import Data.Array qualified as A
import Data.Set qualified as Set
import Debug.Trace

type Machine = (Int, [[Int]], [Int])

parse :: String -> [Machine]
parse = map machine . lines
  where
  machine line = (lights a, map commaList bs, commaList c)
    where
    xs = words line
    Just (a, xs') = uncons xs
    Just (bs, c) = unsnoc xs'
  lights = foldl' (\a b -> shiftL a 1 .|. fromEnum (b == '#')) 0 . reverse . unwrap
  commaList = map read . splitOn "," . unwrap
  unwrap = tail . init

solve1 = sum . map solve1machine'

solve1machine' (target, btns, _) = solve1machine [(0, 0)] Set.empty (target, buttons btns)
  where
  buttons = map $ sum . map bit

solve1machine :: [(Int, Int)] -> Set.Set Int -> (Int, [Int]) -> Int
solve1machine ((x, n):xs) v m@(target, btns)
  | x == target = n
  | x `Set.member` v = solve1machine xs v m
  | otherwise = solve1machine frontier' v' m
  where
  v' = Set.insert x v
  neighbors = map (, n + 1) $ filter (`Set.notMember` v') $ map (xor x) btns
  frontier' = xs ++ neighbors

mkArray xs = A.listArray (0, length xs - 1) xs

solve2 = sum . map solve2machine'
solve2machine' :: Machine -> Int
solve2machine' (_, btns, joltage) = traceShowId $ solve2machine [(allZeros, 0)] Set.empty (btns, mkArray joltage)
  where
  allZeros = mkArray (replicate (length joltage) 0)

solve2machine ((x, n):xs) v m@(btns, target)
  | x == target = n
  | x `Set.member` v = solve2machine xs v m
  | otherwise = solve2machine frontier' v' m
  where
  v' = Set.insert x v
  neighbors = map (, n + 1) $ filter (`Set.notMember` v') $ map inc btns
  isOK a = a `Set.notMember` v' && and (zipWith (<=) (toList a) (toList target))
  inc btn = x A.// [(i, x A.! i + 1) | i <- btn]
  frontier' = xs ++ neighbors

