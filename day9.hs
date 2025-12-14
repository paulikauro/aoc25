{-# LANGUAGE LambdaCase, ViewPatterns #-}
import Data.List.Split (splitOn)
import Data.List (partition, scanl', find, sort)
import Data.Foldable (foldMap)
import Data.Array qualified as A
import Data.Map.Strict qualified as Map
import Data.Bits (bit, (.|.), (.&.), xor, complement, testBit)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Tuple (swap)
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

zipNext xs = zip xs (tail xs)
mkEdges points = (vs, hs, isCCW)
  where
  edges = zipNext (points ++ [head points])
  (vert, horiz) = partition isVert edges
  vs = Map.fromListWith (++) $ map (\((x, y1), (_, y2)) -> (x, [(y1, y2)])) vert
  hs = Map.fromListWith (++) $ map (\((x1, y), (x2, _)) -> (y, [(x1, x2)])) horiz
  (minX, (minY1, minY2)) = second head $ Map.findMin vs
  -- counterclockwise?
  isCCW = minY2 > minY1

-- edge is flipped <=> outside of area points to +x or +y direction from edge
isFlipped ccw ((x1, y1), (x2, y2)) = ccw `xor` if x1 == x2 then y2 > y1 else x2 < x1

isVert :: ((Int, Int), (Int, Int)) -> Bool
isVert = uncurry (==) . (fst . fst &&& fst . snd)

type EMap = Map.Map Int [(Int, Int)]

both f = f *** f

(<**>) :: Applicative f => (a -> f b) -> (c -> f d) -> (a, c) -> f (b, d)
(<**>) g h (a, c) = (,) <$> g a <*> h c

isPointInside :: (EMap, EMap, Bool) -> (Int, Int) -> Bool
isPointInside (vs, hs, ccw) (x, y)
  | isOnEdge vs x y || isOnEdge hs y x = True
  | otherwise = Just True == ((&&) <$> isOK x y vs id <*> isOK y x hs swap)
  where
  isOnEdge m a b = (any (isBetween b) <$> m Map.!? a) == Just True
  isOK a b m f = do
    (l, r) <- (Map.lookupMax <**> Map.lookupMin) $ both (restrict b) $ Map.spanAntitone (< a) m
    pure $ not (isFlipped ccw (expand f l)) && isFlipped ccw (expand f r)

isBetween :: Int -> (Int, Int) -> Bool
isBetween x (a, b) = a <= x && x <= b || a >= x && x >= b

restrict :: Int -> EMap -> Map.Map Int (Int, Int)
restrict b = Map.mapMaybe $ find (isBetween b)

expand f (a, (b1, b2)) = (f (a, b1), f (a, b2))

isSegNotPierced :: (EMap, EMap, Bool) -> ((Int, Int), (Int, Int)) -> Bool
isSegNotPierced (vs, hs, ccw) seg@((x1, y1), (x2, y2))
  | x1 == x2 = test hs y1 y2 x1
  | otherwise = test vs x1 x2 y1
  where
  test m a1 a2 b = Just True == ((\(k, _) -> k >= max a1 a2) <$> (Map.lookupMin $ restrict b $ Map.dropWhileAntitone (\t -> t <= min a1 a2) m))

sortPair (a, b) = (min a b, max a b)

removeOverlaps :: (EMap, EMap, Bool) -> ((Int, Int), (Int, Int)) -> [((Int, Int), (Int, Int))]
removeOverlaps (vs, hs, ccw) seg@((x1, y1), (x2, y2))
  | x1 == x2 = case Map.lookup x1 vs of
    Nothing -> [seg]
    Just ls -> map (reX x1) $ remove' ls (y1, y2)
  | otherwise = case Map.lookup y1 hs of
    Nothing -> [seg]
    Just ls -> map (reY y1) $ remove' ls (x1, x2)
  where
  reX x (a, b) = ((x, a), (x, b))
  reY y (a, b) = ((a, y), (b, y))
  remove' :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
  remove' ls p = go (sort (map sortPair ls)) (sortPair p)
  go [] p = [p]
  go ((a, b):xs) (c, d)
    | a <= c && d <= b = []
    | a <= c = go xs (b + 1, d)
    | d <= b = go xs (c, a - 1)
    | otherwise = go xs (c, a - 1) ++ go xs (b + 1, d)

-- this doesn't work for segments whose length is 3 or smaller,
-- but they don't occurr in the input (at least mine)
isSegInside m (a, b) = isPointInside m a && isPointInside m b && all (isSegNotPierced m) (removeOverlaps m (shrink (a, b)))

shrink ((x1, y1), (x2, y2))
  | x1 == x2 = ((x1, min y1 y2 + 1), (x1, (max y1 y2 - 1)))
  | otherwise = ((min x1 x2 + 1, y1), (max x1 x2 - 1, y1))

isRectInside m ((x1, y1), (x2, y2)) = all (isSegInside m) segs
  where
  segs =
    [ ((x1, y1), (x1, y2))
    , ((x2, y1), (x2, y2))
    , ((x1, y1), (x2, y1))
    , ((x1, y2), (x2, y2))
    ]

-- check point by point, too slow. exists for verification purposes
isSegInside' m ((x1, y1), (x2, y2))
  | x1 == x2 = all (isPointInside m) [(x1, y) | y <- [min y1 y2..max y1 y2]]
  | otherwise = all (isPointInside m) [(x, y1) | x <- [min x1 x2..max x1 x2]]

solve2 ps = maximum . map area . filter (isRectInside m) $ pairs ps
  where
  m = mkEdges ps

main = interact $ show . solve2 . parse
