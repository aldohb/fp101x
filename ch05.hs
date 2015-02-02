factorial :: Int -> Int
factorial n = product [1..n]

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = n * factorial (n - 1)

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs)   = x * product' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x <= y = x:y:ys
    | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []        = []
isort (x:xs)    = insert x (isort xs)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs      = xs
drop' _ []      = []
drop' n (_:xs)  = drop' (n-1) xs

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where smaller = [y | y <- xs, y <= x]
          larger = [z | z <- xs, z > x]

even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n - 1)

evens :: [a] -> [a]
evens []        = []
evens (x:xs)    = x:odds xs

odds :: [a] -> [a]
odds []         = []
odds (_:xs)     = evens xs

