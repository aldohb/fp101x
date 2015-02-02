import Data.Char

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(x,y)] -> [x]
firsts ps = [x | (x, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

factors' :: Int -> [Int]
factors' n = [x | x <- [1..n], mod n x == 0]

prime' :: Int -> Bool
prime' n = factors' n == [1,n]

primes :: Int -> [Int]
primes x = [x | x <- [1..x], prime' x]

find :: Eq a => a -> [(a, b)] -> [b]
find k ps = [v | (k', v) <- ps, k == k']

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [ x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
    where n = length xs - 1

--positions x xs = [n | (x', n) <- zip xs [0..n], x' == x]
--    where n = length xs - 1

lowers :: [Char] -> Int
lowers xs = sum [1 | x <-xs, isLower x ]

count :: Char -> String -> Int
count x xs = sum [1 | x' <- xs, x' == x]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let(mod (let2int c + n) 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n c | c <- xs]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
        where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0 ..25]]
        table' = freqs xs

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects n = [y | y <- [1..n], isPerfect y]
    where isPerfect num = sum (init (factors' num)) == num

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]


riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x, y] | (x, y) <- zip xs ys]

divides :: Int -> Int -> Bool
divides x y = mod x y == 0

divisors :: Int -> [Int]
divisors x = [y | y <- [1..x], divides x y]