putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStr'' :: String -> IO String
putStr'' s = do putStr s
                return s

putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putChar '\n'

getLine' = get []

get :: String -> IO String
get xs = do x <- getChar
            case x of
                '\n' -> return xs
                _ -> get (xs ++ [x])

interact' :: (String -> String) -> IO ()
interact' f = do input <- getLine'
                 putStrLn' (f input)

sequence_' :: Monad m => [m a] -> m ()
sequence_' ms = foldr (>>) (return ()) ms

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m : ms)
  = do a <- m
       as <- sequence' ms
       return (a : as)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (a : as)
  = do b <- f a
       bs <- mapM' f as
       return (b : bs)

isOneM :: Int -> Maybe Bool
isOneM 1 = Just True
isOneM _ = Just False

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p ( x : xs)
  = do flag <- p x
       ys <- filterM' p xs
       if flag then return (x : ys) else return ys

maybePlus :: Int -> Int -> Maybe Int
maybePlus x y = Just (x + y)

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f z [] = return z
foldLeftM f z (x:xs)
  = do y <- f z x
       foldLeftM f y xs

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f z [] = return z
foldRightM f z (x:xs)
  = do y <- foldRightM f z xs
       f x y

readInt :: String -> Int
readInt = read

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >> \ a -> return (f a)