import Data.Bits (shiftL, shiftR, (.|.), (.&.), popCount, complement)
import Data.List (uncons)
import Data.Maybe (fromJust)
import Control.Arrow ((***))

parse1 :: String -> (Integer, [Integer])
parse1 = (\(x:xs) -> (start x, rest xs)) . lines
  where
  start = bittify 'S'
  rest = map (bittify '^')
  bittify c = foldl' step 0
    where
    step a x = shiftL a 1 .|. isC x
    isC = toInteger . fromEnum . (== c)

solve1 (start, lines) = fst $ foldl' step (0, start) lines
  where
  step (counts, curr) line = (counts + popCount hits, result)
    where
    hits = curr .&. line
    new = shiftL hits 1 .|. shiftR hits 1
    result = (curr .|. new) .&. complement hits

parse2 :: String -> ([Int], [[Int]])
parse2 = (bittify 'S' *** map (bittify '^')) . fromJust . uncons . lines
  where
  bittify c = map (fromEnum . (== c))


solve2 (start, lines) = sum $ foldl' step start lines
  where
  step curr line = result
    where
    hits = curr .&. line
    new = shiftL hits .|. shiftR hits
    result = (curr .|. new) .&. complement hits
  (.&.) = zipWith (*)
  (.|.) = zipWith (+)
  shiftL = drop 1 . (++ [0])
  shiftR = ([0] ++)
  complement = map (fromEnum . (== 0))
