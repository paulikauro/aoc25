{-# LANGUAGE LambdaCase #-}
import Data.List.Split (splitOn)
import Data.List (sortOn, sortBy)
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Tuple (swap)
import Control.Arrow ((***))

type P = (Int, Int, Int)

parse :: String -> [P]
parse = map (tup . map read . splitOn ",") . lines
  where
  tup [x, y, z] = (x, y, z)

-- union-find, no path compression, size-based merge

type UF = (Map P P, Map P Int)

find uf@(p, _) k = case p Map.!? k of
  Nothing -> k
  Just k' -> find uf k'

size (_, s) k = Map.findWithDefault 1 k s

union uf@(p, s) k1 k2 = (Map.insert ks kb p, Map.insert kb (s1 + s2) $ Map.delete ks s)
  where
  k1' = find uf k1
  k2' = find uf k2
  s1 = size uf k1'
  s2 = size uf k2'
  f = if s1 < s2 then id else swap -- smaller -> bigger
  ((ks, ss), (kb, sb)) = f ((k1', s1), (k2', s2))

emptyUF = (Map.empty, Map.empty)

both f = f *** f

dist :: Integral a => ((a, a, a), (a, a, a)) -> Double
dist = sqrt . sum . map (^ 2) . uncurry (zipWith (-)) . both (map fromIntegral . untup)
  where
  untup (a, b, c) = [a, b, c]

pairs = \case
  [] -> []
  x:xs -> map (x,) xs ++ pairs xs

solve1 n xs = calc $ snd $ foldl' step emptyUF $ take n edges
  where
  edges :: [(P, P)]
  edges = sortOn dist $ pairs xs
  step :: UF -> (P, P) -> UF
  step uf (a, b) = if a' == b' then uf else union uf a' b'
    where
    a' = find uf a
    b' = find uf b
  calc = product . take 3 . sortBy (flip compare) . toList

solve2 xs = step edges emptyUF
  where
  edges = sortOn dist $ pairs xs
  len = length xs
  step ((a, b):rest) uf
    | isSkip = step rest uf
    | isDone = calcDone a b
    | otherwise = step rest newUF
    where
    a' = find uf a
    b' = find uf b
    isSkip = a' == b'
    newUF = union uf a' b'
    isDone = size newUF (find newUF a') == len
    calcDone (x1, _, _) (x2, _, _) = x1 * x2

