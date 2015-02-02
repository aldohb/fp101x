import Prelude hiding ((!!))

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:replicate(n - 1) x

isElem :: Eq a => a -> [a] -> Bool
isElem _ []         = False
isElem x (y:ys) | x == y = True
                | otherwise = isElem x ys

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n - 1)

and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) | b         = b
            | otherwise = and bs

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] ys = ys
merge' xs [] = xs
merge' (x:xs) (y:ys) | x <= y       = x:merge' xs (y:ys)
                     | otherwise    = y:merge' (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (div (length xs) 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge' (msort zs) (msort ys)
    where (ys, zs) = halve xs
