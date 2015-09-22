module Main where
import Data.Maybe
import Debug.Trace

-- nth triangular number
triangular2 n = sum(take n [1..])
triangular 1 = 1
triangular n = triangular(n-1) + n

-- infinite list of triangular numbers
trilist :: [Int]
trilist = tri 1 2 where
  tri c n = c:(tri (c + n) (n + 1))

-- found = ((1,1),(2,2),(3,2),(4,2),(5,5))
divisorCount :: Int -> Int -> [(Int,Int)] -> Int
divisorCount 1 _ _ = 1
divisorCount n c list -- c = jakaja kanditaatti
  | found > 0    = found
  | mod n c == 0 = 1 + (divisorCount
                        (n `div` c)
                        c
                        (maybeAdd c list))
  | otherwise    = divisorCount n (c + 1) list
    where found = fromMaybe 0 (lookup n list)
          maybeAdd n xs = if any ((n==).(fst)) xs
                          then xs --ok
                          else xs ++ [(c, (divisorCount c 1 xs))]

-- hurts so much
