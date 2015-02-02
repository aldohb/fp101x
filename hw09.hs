{-# LANGUAGE NPlusKPatterns #-}
module Homework09 where

import Prelude hiding (Maybe)
import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero | Succ Nat deriving Show

natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

--natToInteger (Succ n) = natToInteger n + 1
--natToInteger Zero = 0

--natToInteger n = natToInteger n 

--natToInteger (Succ n) = 1 + natToInteger n
--natToInteger Zero = 0

--natToInteger Zero = 1
--natToInteger (Succ n) = (1 + natToInteger n) - 1

--natToInteger = head . m
--    where m Zero = [0]
--          m (Succ n) = [sum [x | x <- (1:m n)]]

--natToInteger = \n -> genericLength [c | c <- show n, c == 'S']

--natToInteger = \n -> length [c | c <- show n, c == 'S']

integerToNat :: Integer -> Nat

integerToNat 0 = Zero
integerToNat (n + 1) = Succ (integerToNat n)

--integerToNat 0 = Succ Zero
--integerToNat n = (Succ (integerToNat n))

--integerToNat n = product [(unsafeCoerce c) :: Integer | c <- show n]

--integerToNat n = integerToNat n

--integerToNat (n + 1) = Succ (integerToNat n)
--integerToNat 0       = Zero

--integerToNat (n + 1) = let m = integerToNat n in Succ m
--integerToNat 0 = Zero

--integerToNat = head . m
--    where {
--          ; m 0 = [0]
--          ; m (n + 1) = [sum [x | x <- (1:m n)]]
--          }

--integerToNat = \n -> genericLength [c | c <- show n, isDigit c]


add :: Nat -> Nat -> Nat

add Zero n = n
add (Succ m) n = Succ (add n m)

--add (Succ m) n = Succ (add n m)
--add Zero n = n

--add Zero n = Zero
--add (Succ m) n = Succ (add m n)

--add (Succ m) n = Succ (add m n)
--add Zero n = Zero

--add n Zero = Zero
--add n (Succ m) = Succ (add n m)

--add n (Succ m) = Succ (add n m)
--add n Zero = Zero

--add n Zero = n
--add n (Succ m) = Succ (add m n)

--add n (Succ m) = Succ (add m n)
--add n Zero = n

mult :: Nat -> Nat -> Nat

--mult Zero Zero = Zero
--mult m (Succ n) = add m (mult m n)

mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

--mult m Zero = Zero
--mult m (Succ n) = add n (mult m n)

--mult m Zero = Zero
--mult m n = add m (mult m (Succ n))

--data Ordering = LT 
--              | EQ
--              | GT

--compare :: (Ord a) => a -> a -> Ordering

--data Tree = Leaf Integer
--          | Node Tree Integer Tree

--occurs :: Integer -> Tree -> Bool

--occurs m (Leaf n) = m == n
--occurs m (Node l n r)
--    = case compare m n of 
--        LT -> occurs m l
--        EQ -> True
--        GT -> occurs m r

--occurs m (Leaf n) = m == n
--occurs m (Node l n r)
--    = case compare m n of
--        LT -> occurs m r
--        EQ -> True
--        GT -> occurs m l

--occurs m (Leaf n) = compare m n
--occurs m (Node l n r)
--    = case compare m n of
--        LT -> occurs m l
--        EQ -> True
--        GT -> occurs m r

--occurs m (Leaf n) = m == n
--occurs m (Node l n r)
--    = case compare m n of
--        LT -> occurs m l
--        EQ -> False
--        GT -> occurs m r

--occurs m (Leaf n) = m == n
--occurs m (Node l n r)
--    | m == n = True
--    | m < n = occurs m l
--    | otherwise = occurs m r

--occurs m (Leaf n) = m == n
--occurs m (Node l n r)
--    | m == n = True
--    | m > n = occurs m l
--    | otherwise = occurs m r

--occurs m n = m == n
--occurs m (Node l n r)
--    | m == n = True
--    | m < n = occurs m l
--    | otherwise = occurs m r

--occurs m n = m == n
--occurs m (Node l n r)
--    | m == n = False
--    | m < n = occurs m r
--    | otherwise = occurs m l

--(Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9)))

data Tree = Leaf Integer 
          | Node Tree Tree deriving Show

balanced :: Tree -> Bool

--leaves (Leaf x)             = x
--leaves (Node l r)           = leaves l + leaves r
--balanced (Leaf _)           = True
--balanced (Node l r)         = abs (leaves l - leaves r) <= 1 || balanced l || balanced r

--leaves (Leaf _) = True
--leaves (Node l r) = leaves l + leaves r
--balanced (Leaf _) = True
--balanced (Node l r) = abs (leaves l - leaves r) <= 1

leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r
balanced (Leaf _) = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r



-- Ex 6
balance :: [Integer] -> Tree

halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
    where (ys, zs) = halve xs

--halve xs = splitAt (length xs / 2) xs
--balance [x] = Leaf x
--balance xs = Node (balance ys) (balance zs)
--    where (ys, zs) = halve xs

--halve xs = splitAt (length xs `div` 2) xs
--balance [x] = Leaf x
--balance xs = Node ys zs
--    where (ys, zs) = balance(halve xs)

--halve xs = splitAt (length xs `div` 2) xs
--balance x = Leaf x
--balance xs = Node (balance ys) (balance zs)
--    where (ys, zs) = halve xs

instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Just (f (Nothing))
    (Just x) >>= f = f x