import Data.List.Split (splitOn)
import Control.Arrow ((***))

parse :: String -> ([(Int, Int)], [Int])
parse = (map range *** map read) . tup . map lines . splitOn "\n\n"
  where
  range = tup . map read . splitOn "-"
  tup [a, b] = (a, b)

solve1 (ranges, ids) = length $ filter isFresh ids
  where
  isFresh id = any (inRange id) ranges
  inRange id (a, b) = a <= id && id <= b

