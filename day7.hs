import Data.Array qualified as A
import Data.Bits (shiftL, shiftR, (.|.), (.&.), popCount, complement)

mkArray xs = A.listArray (0, length xs - 1) xs

parse :: String -> (Integer, A.Array Int Integer)
parse = (\(x:xs) -> (start x, rest xs)) . lines
  where
  start = bittify 'S'
  rest = mkArray . map (bittify '^')
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
