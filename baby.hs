take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ []  = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
    | x == y = True
    | otherwise = elem' x ys

qsort' :: (Ord a) => [a] -> [a]
qsort' [] = []
qsort' [x] = [x]
qsort' (x:xs) =
    let small = qsort' [y | y <- xs, y <= x]
        big = qsort' [y | y <- xs, y > x]
    in small ++ [x] ++ big

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith f xs ys)

spell :: String -> String
spell [] = []
spell [x] = [x]
spell (x:xs) = x : '-' : spell xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n : collatz (n `div` 2)
    | otherwise = n : collatz (3*n + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map collatz [1..100]))
    where isLong xs = length xs > 15

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x xs = foldl (||) False (map (==x) xs)

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> (f x):acc) []