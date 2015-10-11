module Main where
import Data.Maybe
import Debug.Trace
import Data.List
import Data.Char
import Data.List.Split

-- Complete euler numbers: 20, 27, 34
-- Incomplete euler numbers: 12

-- Euler 12

-- nth triangular number
--triangular2 n = sum(take n [1..])
--triangular 1 = 1
--triangular n = triangular(n-1) + n
-- infinite list of triangular numbers
triangular :: Int -> Int
triangular n = let m = fromIntegral n in (m^2+m) `div` 2
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
        listPrimes' _ _ = error "Hiljaa, kaantaja"

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
_divisorCount :: Int -> Int -> [(Int,Int)] -> Int
_divisorCount 1 _ _ = 1
_divisorCount n c list -- c = divisor canditate
  | found > 0    = found
  | mod n c == 0 = trace ("kutsu arvoilla " ++ show n ++" " ++ show c ++ " " ++ (show list))
                   (1 + (_divisorCount
                        (n `div` c)
                        c
                        (maybeAdd c list)))
  | otherwise    = _divisorCount n (c + 1) list
    where found = fromMaybe 0 (lookup n list)
          maybeAdd m xs = if any ((m==).(fst)) xs
                          then xs --ok
                          else xs ++ [(c, (_divisorCount c 2 xs))]


-- palauttaa alkujakajat ja niiden maarat
primeDivisors :: Int -> [(Int,Int)]
primeDivisors x | x < 2 = []
                | otherwise = divisors' x 2 []
  where divisors' :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
        divisors' n c list | n `mod` c == 0 = divisors' (n `div` c) c (maybeAdd c list)
                           | c > (floor $ sqrt (fromIntegral n)) = maybeAdd n list
                           | c > 2 = divisors' n (c+2) list
                           | otherwise = divisors' n (c+1) list
        maybeAdd :: Int -> [(Int,Int)] -> [(Int,Int)]
        maybeAdd 1 l = l
        maybeAdd n [] = [(n,1)]
        maybeAdd n ((x,y):xs) | x == n = (x,(y+1)):xs
                              | otherwise =  (x,y):(maybeAdd n xs)
        
factorCount :: Int -> Int
factorCount n = foldr ((*) . (1+) . snd) 1 (primeDivisors n)

_euler12 :: Int -> Int
_euler12 x = findFirstSuch [1..x]
  where over500DivisorsP = (> 500) . factorCount . triangular
        findFirstSuch [] = -1
        findFirstSuch (x:xs) | over500DivisorsP x = triangular x
                             | otherwise = findFirstSuch xs

-- Euler 20 - digit factorials
_euler20 = sum $ map digitToInt $ show $ product [1..100]
_euler20b = sum . map digitToInt . show $ product [1..100]

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

-- Euler 22

stringSum :: String -> Int
stringSum s = foldr ((+) . (-64+) . ord) 0 s

-- summaa sanan numerolla kerrottuna sana summa
-- aloitusindexin numero sanalista, numero
sumAndMultiplyByLineNum :: Int -> [String] -> Int
sumAndMultiplyByLineNum _ [] = 0
sumAndMultiplyByLineNum n (x:xs) = n * stringSum x +
                                   sumAndMultiplyByLineNum (n+1) xs

_euler22 = sumAndMultiplyByLineNum 1 . dropWhile (==[]) . sort . splitOneOf ",\""

_euler22IO :: FilePath -> IO ()
_euler22IO path = readFile path >>= putStrLn . show . _euler22

_testEuler22 =  readFile "euler-data/p022_names.txt" >>=
                putStrLn . show . _euler22

_test = readFile "euler-data/p022_names.txt" >>=
                putStrLn . read


-- Euler 78
-- rekursiivisesti laske kolikoiden kombinaatiomaarat
-- tallenna maarat listaan parina [(5,7)]piccolo



-----------------------------------------------------------------------------

main = do
  let a = 100000
  print a        
