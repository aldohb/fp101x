double x      = x + x
quadruple x   = double (double x)

factorial n   = product [1..n]
average ns    = div (sum ns) (length ns)

last' []      = error "empty list"
last' xs      = head (reverse xs)

last'' []     = error "empty list"
last'' [x]    = x
last'' (x:xs) = last'' xs

init' []      = error "empty list"
init' xs      = take (length xs - 1) xs

init'' []     = error "empty list"
init'' xs     = reverse (tail (reverse xs))

product' []       = error "empty list"
product' [x]      = x
product' (x:xs)   = x * (product xs)

--qsort []      = []
--qsort (x:xs)  = qsort smaller ++ [x] ++ qsort larger
--                where
--                  smaller = [a | a <- xs, a < x]
--                  larger  = [b | b <- xs, b > x]

qsort' []       = []
qsort' (x:xs)   = reverse (qsort' smaller ++ [x] ++ qsort' larger)
                  where
                    smaller = [a | a <- xs, a <= x]
                    larger  = [b | b <- xs, b > x]

--qsort'' []      = []
--qsort'' (x:xs)  = qsort'' larger ++ [x] ++ qsort'' smaller
--                  where
--                    smaller = [a | a <- xs, a < x]
--                    larger  = [b | b <- xs, b >= x]

mylast xs = drop (length xs - 1) xs

myinit xs = take (length xs) xs


--qsort []        = []
--qsort (x:xs)    = reverse (qsort smaller ++ [x] ++ qsort larger)
--    where smaller = [a | a <- xs, a <= x]
--          larger  = [b | b <- xs, b > x]