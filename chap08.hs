import System.IO 

--(>>=) :: IO a -> (a -> IO b) -> IO b
--p >>= f =  P (\inp ->
--               case parse p inp of
--                   [(v, out)] -> parse (f v) out
--                   [] -> [])

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it: "
             guess word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '_'
                    xs <- sgetLine
                    return (x:xs)

guess :: String -> IO ()
guess word = do putStr "> "
                xs <- getLine
                if xs == word then
                    putStrLn "You got it!"
                else
                    do putStrLn (diff word xs)
                       guess word

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

getLine' :: IO String
getLine' = get []

get :: String -> IO String
get xs
    = do x <- getChar
         case x of
              '\n' -> return xs
              _ -> get (xs ++ [x])

interact' :: (String -> String) -> IO ()
interact' f
    = do input <- getLine'
         putStrLn' (f input)

sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' ms = foldr (>>) (return ()) ms

sequence' [] = return []
sequence' (m:ms)
    = m >>=
        \a ->
        do as <- sequence' ms
           return (a:as)

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x:xs)
    = do flag <- p x
         ys <- filterM' p xs
         if flag then return ys else return (x:ys)

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = 
foldLeftM f a (x:xs) = foldLeftM f (return f a x) xs