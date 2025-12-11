import Data.List.Split (splitOn)
import Control.Arrow (second)
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Set qualified as Set

parse = map (second words . tup . splitOn ": ") . lines
  where
  tup [x, y] = (x, y)

solve1 ms = paths ms "you" "out" []

paths ms' a b forbiddens = p Map.! b
  where
  isAllowed x = x `notElem` forbiddens
  ms = map (\(a, bs) -> (a, filter isAllowed bs)) $ filter (\(a, bs) -> isAllowed a) ms'
  nodes = Set.fromList $ map fst ms ++ concatMap snd ms
  rev = Map.fromListWith (++) $ concatMap (\(a, bs) -> map (\b -> (b, [a])) bs) ms
  p = Map.fromList [(x, f x) | x <- toList nodes]
  f x
    | x == a = 1
    | otherwise = sum [p Map.! pred | pred <- Map.findWithDefault [] x rev]

solve2 ms = (fft_dac + dac_fft) :: Integer
  where
  p = paths ms
  fft_dac = p "svr" "fft" ["dac"] * p "fft" "dac" [] * p "dac" "out" ["fft"]
  dac_fft = p "svr" "dac" ["fft"] * p "dac" "fft" [] * p "fft" "out" ["dac"]
