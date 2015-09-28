module Main where
import Data.Maybe
import Debug.Trace
import Data.List
import Data.Char


-- Euler 12

-- nth triangular number
triangular2 n = sum(take n [1..])
triangular 1 = 1
triangular n = triangular(n-1) + n

-- infinite list of triangular numbers
trilist :: [Int]
trilist = tri 1 2 where
  tri c n = c:(tri (c + n) (n + 1))


-- list of primes, up to 2^63
listPrimes :: [Int]
listPrimes = 2:(listPrimes' 3 [2])
  where listPrimes' :: Int -> [Int] -> [Int]
        listPrimes' c (x:xs) = if isPrime' (x:xs) c
                               then c:(listPrimes' (c+2) ((x:xs) ++ [c])) -- vika
                               else listPrimes' (c+2) (x:xs)


isPrime' :: [Int] -> Int -> Bool -- list of preceding primes, prime canditate
isPrime' _ 2  = True
isPrime' (x:xs) c | c < 2 = False
                 | c `mod` x == 0 = False
                 | fromIntegral x > sqrt (fromIntegral c) = True
                 | otherwise = (isPrime' xs c)
isPrime' _ _ = error "Not enough preceding primes."


isPrime :: Int -> Bool
isPrime n = isPrime' listPrimes n
  


-- found = ((1,1),(2,2),(3,2),(4,2),(5,1)) -> 1*2*2*3*3*4*4*5
divisorCount :: Int -> Int -> [(Int,Int)] -> Int
divisorCount 1 _ _ = 1
divisorCount n c list -- c = divisor canditate
  | found > 0    = found
  | mod n c == 0 = trace ("kutsu arvoilla" ++ show n ++ show c)
                   (1 + (divisorCount
                        (n `div` c)
                        c
                        (maybeAdd c list)))
  | otherwise    = divisorCount n (c + 1) list
    where found = fromMaybe 0 (lookup n list)
          maybeAdd n xs = if any ((n==).(fst)) xs
                          then xs --ok
                          else xs ++ [(c, (divisorCount c 2 xs))]


--nub -> remove duplicates
-- palauttaa jakajat ja niiden maarat
divisors :: Int -> [(Int,Int)]
divisors x = divisors' x 2 []

  where divisors' n c list | n `mod` c == 0 = divisors' (n `div` c) c (maybeAdd c list)
                           | otherwise = divisors' n (c+2) list
        maybeAdd n [] = [(n,1)]
        maybeAdd n ((x,y):xs) | x == n = (x,(y+1)):xs
                              | otherwise =  (x,y):(maybeAdd n xs)

{-
   divisors n c list -- c = divisor canditate
     | mod n c == 0 = (1 + (divisors -- on jaollinen luvulla
                            (n `div` c)
                            c
                            (maybeAdd c list)))
     | otherwise  = divisors n (c + 1) list
-}

-- Euler 20 - digit factorials
_euler20 = sum $ map digitToInt $ show $ product [1..100]

-- Euler 27

quadradicPrimes = maximum $ map (\(a,b) -> ((length (consecutivePrimes a b)),a,b)) coefficients
  where
    consecutivePrimes a b = takeWhile (isPrime . (\n -> n^2 + a*n + b)) [0..]
    coefficients = [(a,b) | a <- [-999..999], b <- [-999..999]]

_euler27 = let (_,a,b) = quadradicPrimes in a*b


-- Euler 34

factorial :: Integral a => a -> a
factorial n = product [1..n]

isFacSum :: Integer -> Bool
isFacSum n = n == fromIntegral (sum $ map (factorial . digitToInt) $ show n)

digitFactorials n = sum $ filter isFacSum [1..n]

_euler34 = digitFactorials 962845 - 3
