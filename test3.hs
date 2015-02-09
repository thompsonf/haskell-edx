{-# LANGUAGE NPlusKPatterns #-}

import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat (n+1) = Succ (integerToNat n)

add :: Nat -> Nat -> Nat
add n (Succ m) = Succ (add m n)
add n Zero = n

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

data Tree1 = Leaf1 Integer
           | Node1 Tree1 Integer Tree1

occurs :: Integer -> Tree1 -> Bool
occurs m (Leaf1 n) = m == n
occurs m (Node1 l n r)
  | m == n = True
  | m < n = occurs m l
  | m > n = occurs m r

data Tree = Leaf Integer
          | Node Tree Tree

balanced :: Tree -> Bool
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r
balanced (Leaf _) = True
balanced (Node l r)
  = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

balance :: [Integer] -> Tree
halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
  where (ys, zs) = halve xs

data Expr = Add Expr Expr | Val Int

data Tree2 = Leaf2 Int | Node2 Tree2 Tree2