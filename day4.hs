{-# LANGUAGE LambdaCase #-}
import Data.Array qualified as A

parse xs = (A.listArray ((1, 1), (h, w)) $ concat l, h, w)
  where
  l = lines xs
  h = length l
  w = length (head l)

solve1 (arr, h, w) = sum $ map thing $ A.range (A.bounds arr)
  where
  thing c = if isPaper c == 1 && accessible c then 1 else 0
  -- 5 because we're counting the paper roll itself too
  accessible = (< 5) . sum . map isPaper . surrounded
  surrounded (y, x) = [(y', x') | y' <- [y - 1, y, y + 1], x' <- [x - 1, x, x + 1]]
  isPaper = (\case { '.' -> 0; '@' -> 1; }) . get
  get (y, x)
    | y < 1 || x < 1 = '.'
    | y > h || x > w = '.'
    | otherwise = arr A.! (y, x)

