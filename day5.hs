import Data.List.Split (splitOn)
import Control.Arrow ((***))
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Monad.State
import Data.Array qualified as A
import Data.Maybe (catMaybes)
import Data.Foldable (toList)
import Control.Monad

parse :: String -> ([(Int, Int)], [Int])
parse = (map range *** map read) . tup . map lines . splitOn "\n\n"
  where
  range = tup . map read . splitOn "-"
  tup [a, b] = (a, b)

solve1 (ranges, ids) = length $ filter isFresh ids
  where
  isFresh id = any (inRange id) ranges

inRange id (a, b) = a <= id && id <= b
overlaps (a1, b1) (a2, b2) = b1 >= a2 && b2 >= a1
merge (a1, b1) (a2, b2) = (min a1 a2, max b1 b2)
arrIndices = A.range . A.bounds

dfs :: A.Array Int (Int, Int) -> Int -> State (Set Int) (Maybe (Int, Int))
dfs g n = do
  v <- isVisited n
  if v
    then pure Nothing
    else do
      markVisited n
      let this = g A.! n
      ns <- catMaybes <$> traverse (dfs g) (neighbors n g)
      pure $ Just $ foldl' merge this ns
  where
  isVisited n = Set.member n <$> get
  markVisited n = modify' (Set.insert n)
  neighbors n g = filter (overlaps (g A.! n) . (g A.!)) (arrIndices g)

solve2 (ranges, _) = sum $ map rangeLen connectedComponents
  where
  g = A.listArray (1, length ranges) ranges
  connectedComponents =
    flip evalState Set.empty (catMaybes <$> traverse (dfs g) (arrIndices g))
  rangeLen (a, b) = b - a + 1
