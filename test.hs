import Data.Char

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect n = n == (sum . init . factors $ n)

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = length xs - 1

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

let2int :: Char -> Int
let2int c
  | isLower c = ord c - ord 'a'
  | otherwise = ord c - ord 'A'

int2lowerlet :: Int -> Char
int2lowerlet n = chr (ord 'a' + n)

int2upperlet :: Int -> Char
int2upperlet n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2lowerlet ((let2int c + n) `mod` 26)
  | isUpper c = int2upperlet ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], x `divides` d]

myand [] = True
myand (b:bs) = and bs && b

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs