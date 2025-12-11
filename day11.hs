import Data.List.Split (splitOn)
import Control.Arrow (second)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Debug.Trace

parse = map (second words . tup . splitOn ": ") . lines
  where
  tup [x, y] = (x, y)

solve1 = bfs ["you"] Set.empty (Map.singleton "you" 1) . Map.fromList
  where
  bfs [] v c m = c Map.! "out"
  bfs (node:ns) v c m
    -- | traceShow (node, ns, v, c) False = undefined
    | node `Set.member` v = bfs ns v c m
    | otherwise = bfs (ns ++ frontier) v' c' m
    where
    count = Map.findWithDefault 0 node c
    v' = Set.insert node v
    neighbors = Map.findWithDefault [] node m
    frontier = filter (`Set.notMember` v) neighbors
    c' = foldl' (\a b -> upc a b count) c neighbors
    upc c node count = Map.insertWith (+) node count c
