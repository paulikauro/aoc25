{-# LANGUAGE LambdaCase #-}
import Data.Array qualified as A
import Data.Array.ST qualified as ST
import Data.STRef qualified as ST
import Control.Monad.ST (ST)
import Control.Monad.ST qualified as ST
import Control.Monad (filterM, when)
import Data.Functor (($>))

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

neighbors (y, x) = [(y - 1, x - 1), (y - 1, x), (y - 1, x + 1), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x), (y + 1, x + 1)]

whileM :: Monad m => m Bool -> m () -> m ()
whileM test body = do
  t <- test
  when t $ do
    body
    whileM test body

solve2 (arr', h, w) = ST.runST $ do
    arr <- ST.thaw arr' :: ST s (ST.STUArray s (Int, Int) Char)
    let
      get c
        | A.inRange (A.bounds arr') c = ST.readArray arr c
        | otherwise = pure '.'
      isPaper = fmap (== '@') . get
      isAccessible = fmap ((< 4) . length) . filterM isPaper . neighbors
      isOK c = (&&) <$> isPaper c <*> isAccessible c
    stack <- filterM isOK (A.range $ A.bounds arr') >>= ST.newSTRef
    count <- ST.newSTRef 0
    let pop = ST.readSTRef stack >>= \(x:xs) -> ST.writeSTRef stack xs $> x
    whileM ((/= []) <$> ST.readSTRef stack) $ do
      a <- pop
      i <- isPaper a
      when i $ do
        ST.modifySTRef' count (+ 1)
        ST.writeArray arr a '.'
        new <- filterM isOK $ neighbors a
        ST.modifySTRef' stack (new ++)
    ST.readSTRef count

