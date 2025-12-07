import Data.Bits (shiftL, shiftR, (.|.), (.&.), popCount, complement)
import Data.List (uncons)
import Data.Maybe (fromJust)

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

parse2 :: String -> (String, [String])
parse2 = fromJust . uncons . lines

solve2 (start, lines) = sum $ foldl' step (map (fromEnum . (== 'S')) start) lines
  where
  step curr line = result
    where
    hits = zipWith (\a x -> if x == '^' then a else 0) curr line
    new = zipWith (+) (tail hits ++ [0]) ([0] ++ hits)
    result = zipWith (\x h -> if h > 0 then 0 else x) (zipWith (+) curr new) hits
