import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

up2int :: Char -> Int
up2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2up :: Int -> Char
int2up n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let (mod (let2int c + n) 26)
    | isUpper c = int2up (mod (up2int c + n) 26)
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]