import Prelude hiding ((&&))
--abs' :: Int -> Int
--abs' n = if n >= 0 then n else - n

--signum' :: Int -> Int
--signum' n = if n < 0 then (-1) else
--				if n == 0 then 0 else 1

abs' n | n >= 0     = n
       | otherwise  = (-n)

signum' :: Int -> Int

signum' n | n < 0       = (-1)
          | n == 0      = 0
          | otherwise   = 1

const' :: a -> b -> a
const' x = \_ -> x

{-odds' :: Int -> [Int]
odds' n = map f [0..n-1]
          where f x = x * 2 + 1 -}

odds' :: Int -> [Int]
odds' n = map(\x -> x * 2 + 1) [0..n-1]

--halve :: [a] -> ([a],[a])
--halve xs | len == 0               = error "empty list"
--         | (mod len 2) /= 0       = error "Not even length"
--         | otherwise              = (take n xs, drop n xs)
--         where n        = (div (length xs) 2)
--               len      = length xs

--safetail :: [a] -> [a]
--safetail []         = []
--safetail (x:xs)     = xs

--safetail' :: [a] -> [a]
--safetail' xs | null xs           = []
--             | otherwise         = drop 1 xs

--safetail'' :: [a] -> [a]
--safetail'' xs = if null xs then [] else drop 1 xs

or'     :: Bool -> Bool -> Bool
or' True True  = True
or' True False = True
or' False True = True
or' False False = False

or''    :: Bool -> Bool -> Bool
or'' True _ = True
or'' _ True = True
or'' False False  = False

or''' :: Bool -> Bool -> Bool
or''' False False = False
or''' _ _ = True

--and' :: Bool -> Bool -> Bool
--and' x y = if (x == True) && (y == True) then True else False

and'' :: Bool -> Bool -> Bool
and'' x y = if (x == True) then y else
                if y == False then y else True

mult' :: Num a => a -> (a -> (a -> a))
mult' = \x -> (\y -> (\z -> x * y * z))

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where n = length xs `div` 2

safetail :: [a] -> [a]
safetail = \xs -> 
    case xs of
        [] -> []
        (_:xs) -> xs

funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs

(&&) :: Bool -> Bool -> Bool
a && b = if b then a else False

e3 [x, y] = (x, True)

e8 x y = (y, x)

e4 (x, y) = x

e7 (x, y) = (y, x)

e6 x y = x * y