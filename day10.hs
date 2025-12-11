{-# LANGUAGE BlockArguments, LambdaCase #-}
import Data.Bits (shiftL, (.|.), xor, bit, popCount, testBit)
import Data.List.Split (splitOn)
import Data.List (uncons, unsnoc, intercalate, sortOn)
import Data.Foldable (toList)
import Data.Array qualified as A
import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Debug.Trace
import Control.Arrow (second)
import Control.Monad (guard)

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

div' n = \case
  0 -> []
  1 -> [[n]]
  k -> [0..n] >>= \i -> map (i:) $ div' (n - i) (k - 1)

main = interact $ show . solve2 . parse
solve2 = sum . map solve2machine'
solve2machine' :: Machine -> Int
solve2machine' ((_, btns, joltage) :: Machine) = traceShowId $ solve2machine (length btns) jolts
  where
  jolts = sortOn (popCount . snd) $ map (second mask) $ zip joltage [0..]
  mask j = foldl' (\a b -> shiftL a 1 .|. fromEnum (j `elem` b)) (0 :: Int) $ reverse btns

solve2machine nBtns jolts = minimum $ go (allZeros nBtns) jolts
  where
  go :: A.Array Int (Maybe Int) -> [(Int, Int)] -> [Int]
  go m [] = [sum $ catMaybes $ toList m]
  go m ((j, bs):js) = do
    let fixed = calcFixed bs m
    guard $ fixed <= j
    guard $ areOK js m
    let available = j - fixed
    let slots' = slots bs m
    let k = length slots'
    --trace (replicate (length jolts - length js) '\t' ++ show ("j:js m", j, js, m, "fix av k", fixed, available, k)) [()]
    guard $ k > 0 || fixed == j
    if k == 0
      then go m js
      else do
        d <- div' available k
        go (m A.// zip slots' (map Just d)) js
  calcFixed bs m = sum [c | (i, Just c) <- A.assocs m, testBit bs i]
  slots bs m = [i | (i, Nothing) <- A.assocs m, testBit bs i]
  areOK js m = all (\(j, bs) -> calcFixed bs m <= j) js
  allZeros n = mkArray $ replicate n Nothing

