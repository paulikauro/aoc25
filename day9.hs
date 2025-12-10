{-# LANGUAGE LambdaCase #-}
import Data.List.Split (splitOn)
import Data.List (partition, scanl')
import Data.Array qualified as A
import Data.Map.Strict qualified as Map
import Data.Bits (bit, (.|.), (.&.), xor, complement, testBit)
import Control.Arrow (second, (&&&), (***))
import Debug.Trace

parse :: String -> [(Int, Int)]
parse = map (tup . map read . splitOn ",") .lines
  where
  tup [x, y] = (x, y)

pairs = \case
  [] -> []
  x:xs -> map (x,) xs ++ pairs xs

solve1 :: [(Int, Int)] -> Int
solve1 = maximum . map area . pairs

area ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

edges xs = zip xs (tail xs)

-- breaks if lo=0
mkLine (lo, hi) = to hi - to (lo - 1)
  where
  to x = bit (x + 1) - 1

mkEnds (lo, hi) = bit lo .|. bit hi

-- (i, lo, hi)
table :: [(Int, (Int, Int))] -> A.Array Int Integer
table es = A.listArray (0, 100000) $ map fst $ scanl' step (0, 0) [1..100000]
  where
  edgeMap = Map.fromListWith (.|.) $ map (second mkLine) es
  vertexMap = Map.fromListWith (.|.) $ map (second mkEnds) es
  step (last, lateflip) i = (new, lateflip')
    where
    e = Map.findWithDefault 0 i edgeMap
    nowflip = e `nimp` last
    lateflip' = e .&. last
    new = (last `xor` lateflip `xor` nowflip) .|. Map.findWithDefault 0 i vertexMap

--nimp :: Bits a => a -> a -> a
nimp a b = a .&. complement b

mkTables :: [(Int, Int)] -> (A.Array Int Integer, A.Array Int Integer)
mkTables = (tableV *** tableH) . partition (uncurry (==) . (fst . fst &&& fst . snd)) . edges
  where
  tableV = table . map (\((x, a), (_, b)) -> (x, sort' a b))
  tableH = table . map (\((a, y), (b, _)) -> (y, sort' a b))

sort' a b
  | a <= b = (a, b)
  | otherwise = (b, a)

printTable tbl w h i
  | h == i = pure ()
  | otherwise = do
    printRow (tbl A.! i) w
    putStrLn ""
    printTable tbl w h (i + 1)
  where
  printRow r i
    | i == 0 = pure ()
    | otherwise = do
      putStr $ case testBit r i of { True -> "X"; False -> "." }
      printRow r (i - 1)

solve2 ps = maximum $ map area $ filter isContained $ pairs ps
  where
  (v, h) = mkTables ps
  isContained = all isSegmentContained . segments
  isSegmentContained ((x1, y1), (x2, y2))
    | x1 == x2 = isSegmentContained' v x1 $ sort' y1 y2
    | otherwise = isSegmentContained' h y1 $ sort' x1 x2
  isSegmentContained' tbl i p = let x = mkLine p in x == (tbl A.! i) .&. x
  segments ((x1, y1), (x2, y2)) =
    [ ((x1, y1), (x1, y2))
    , ((x2, y1), (x2, y2))
    , ((x1, y1), (x2, y1))
    , ((x1, y2), (x2, y2))
    ]

