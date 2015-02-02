import Prelude hiding (Left, Right, Up, Down, flip, Maybe, Nothing, Just)

type Board = [Pos]
type Pos = (Int, Int)

data Move = Left | Right | Up | Down

move                :: Move -> Pos -> Pos
move Left (x, y)    = (x - 1, y)
move Right (x, y)   = (x + 1, y)
move Up (x, y)      = (x, y - 1)
move Down (x, y)    =  (x, y + 1)

moves               :: [Move] -> Pos -> Pos
moves [] p          = p
moves (m:ms) p      = moves ms (move m p)

flip                :: Move -> Move
flip Left           = Right
flip Right          = Left
flip Up             = Down
flip Down           = Up

data Shape = Circle Float | Rect Float Float

square              :: Float -> Shape
square n            = Rect n n

area                ::Shape -> Float
area (Circle r)     = pi * r ^ 2
area (Rect x y)     = x * y

data Maybe a = Nothing | Just a

safediv             :: Int -> Int -> Maybe Int
safediv _ 0         = Nothing
safediv m n         = Just (div m n)

safehead            :: [a] -> Maybe a
safehead []         = Nothing
safehead xs         = Just (head xs)

data Nat = Zero | Succ Nat deriving Show

nat2int             :: Nat -> Int
nat2int Zero        = 0
nat2int (Succ n)    = 1 + nat2int n

int2nat             :: Int -> Nat
int2nat 0           = Zero
int2nat n           = Succ (int2nat (n - 1))

add                 :: Nat -> Nat -> Nat
-- add m n             = int2nat (nat2int m + nat2int n)  -- inefficient
add Zero n           = n
add (Succ m) n       = Succ (add m n)