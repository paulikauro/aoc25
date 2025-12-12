{-# LANGUAGE BlockArguments, LambdaCase #-}
import Data.Bits (shiftL, (.|.), (.&.), xor, bit, popCount, testBit)
import Data.List.Split (splitOn)
import Data.List (uncons, unsnoc, intercalate, sortOn, find, sortBy, foldl1', transpose, findIndex)
import Data.Foldable (toList, traverse_)
import Data.Function (on)
import Data.Array qualified as A
import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust)
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

main = interact $ show . solve2 . parse
solve2 = sum . map solve2machine'
solve2machine' :: Machine -> Int
solve2machine' = traceShowId . solve2machine . toAMatrix . gj . toMatrix

solve2machine :: ([Int], ([[Int]], [[Int]])) -> Int
solve2machine (jolts, (unknowns, diag)) = minimum $ go_unknowns unknowns []
  where
  allCols = unknowns ++ diag
  h = length jolts
  go_unknowns [] btns = go_diag diag btns
  go_unknowns (x:xs) btns = do
    btn <- [0..256]
    go_unknowns xs (btns ++ [btn])
  go_diag :: [[Int]] -> [Int] -> [Int]
  go_diag [] btns = [sum btns]
  go_diag (x:xs) btns = if r /= 0 || q < 0 then [] else go_diag xs (btns ++ [q])
    where
    Just (pi, p) = find ((/= 0) . snd) $ zip [0..] x
    (q, r) = available btns pi `divMod` p
  available btns pi = (jolts !! pi) - fixed btns pi
  fixed btns pi = sum $ zipWith (\b c -> b * (c !! pi)) btns allCols

gj :: [[Int]] -> [[Int]]
gj xs = descSort $ normRes $ foldl' step xs [0..length xs - 1]
  where
  step a ri = case pivot of
    Nothing -> a
    Just (pi, p) -> [if i == ri then row else elimRow er pi p row | (i, er) <- zip [0..] a]
    where
    row = a !! ri
    pivot = find ((/= 0) . snd) $ zip [0..] row
  elimRow er pi p pr = zipWith (+) (map (* erCoeff) er) $ map (* pCoeff) pr
    where
    erp = er !! pi
    g = gcd erp p
    pCoeff = negate (erp `div` g)
    erCoeff = p `div` g
  normRes = filter (any (/= 0)) . map normRow
  normRow xs = map (\x -> (x `div` g) * signum' (last xs)) xs
    where
    g = if nonzero == [] then 1 else foldl1' gcd nonzero
    nonzero = filter (/= 0) xs
  -- 0 == signum 0 /= signum' 0 == 1
  signum' x = if x < 0 then -1 else 1
  descSort = sortBy (flip compare `on` map abs)

toMatrix (_, btns, jolts) = zipWith (\j ji -> map (fromEnum . elem ji) (reverse btns) ++ [j]) jolts [0..]

toAMatrix :: [[Int]] -> ([Int], ([[Int]], [[Int]]))
toAMatrix = second (splitDiag . sortCols) . uncons' . transpose . map reverse
  where
  uncons' = fromJust . uncons
  splitDiag xs = splitAt extraUnknowns xs
    where
    unknowns = length xs
    eqs = length (head xs)
    extraUnknowns = unknowns - eqs
  sortCols = sortOn f
  f col = if length (filter (/= 0) col) /= 1 then -1 else fromJust $ findIndex (/= 0) col

printArr :: [[Int]] -> IO ()
printArr xs = traverse_ print xs >> putStrLn ""

printAArr (j, (u, d)) = printArr . transpose $ [j] ++ u ++ d

