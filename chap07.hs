import Prelude hiding (any)
import Data.Char

add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

--map' :: (a -> b) -> [a] -> [b]
--map' f xs = [f x | x <- xs]

--map'' :: (a -> b) -> [a] -> [b]
--map'' f []      = []
--map'' f (x:xs)  = f x : map'' f xs

--filter' :: (a -> Bool) -> [a] -> [a]
--filter' p xs = [x | x <- xs, p x]

--filter'' :: (a -> Bool) -> [a] -> [a]
--filter'' p []   = []
--filter'' p (x:xs) | p x         = x:filter'' p xs
--                  | otherwise   = filter'' p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []       = v
foldr' f v (x:xs)   = f x (foldr f v xs)

sum' = foldr' (+) 0
product = foldr' (*) 1
and' = foldr' (&&) True
or' = foldr' (||) False

length' :: [a] -> Int
length' = foldr' (\_ n-> 1 + n) 0

reverse' :: [a] -> [a]
reverse' = foldr' (\x xs -> xs ++ [x]) []

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (&&) True . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x:takeWhile' p xs
    | otherwise = takeWhile' p xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = x:xs

dropWhile''' p = foldl add []
    where add [] x = if p x then [] else [x]
          add acc x = x:acc

filter'' p = foldr (\ x xs -> if p x then x : xs else xs) []

dec2int :: [Integer] -> Integer
dec2int = foldl (\x y -> x * 10 + y) 0


compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

mysum (x,y) = x + y

mysum' = curry' mysum
mysum'' = curry mysum

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f = \ (x, y) -> f x y

summy = uncurry' mysum'


----------------
--------------
---------------
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = mod n 2:int2bin (div n 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits:chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id


unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x 
    | p x = []
    | otherwise = h x :unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

chop8' :: [Bit] -> [[Bit]]
chop8'= unfold null (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail
--map' f = unfold empty (f .head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

ex30 p f xs = filter p (map f xs)
ex30' p f xs = [f x | x <- xs, p (f x)]

ex18 xs ys = (reverse xs) ++ ys
ex18' xs ys = ys ++ (reverse xs)

any :: (a -> Bool) -> [a] -> Bool
any p = not . null . dropWhile (not . p)