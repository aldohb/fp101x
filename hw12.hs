--import Prelude hiding ((++))

--last' :: [a] -> a
--last' (_:xs) = last' xs
--last' [x] = x

--foldr' :: (a -> b -> b) -> b -> [a] -> b
--foldr' f v (x:xs) = f x (foldr f v xs)
--foldr' _ v [] = v

--init' :: [a] -> [a]
--init' (x:xs) = x:init' xs
--init' [_] = []

--drop' :: Int -> [a] -> [a]
--drop' 0 xs = xs
--drop' n [] = []
--drop' n (_:xs) = drop' (n-1) xs

--(++) :: [a] -> [a] -> [a]

--(x:xs) ++ ys = x : (xs ++ ys)
--[] ++ ys = ys

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs