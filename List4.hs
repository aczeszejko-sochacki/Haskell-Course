-- Aleksander Czeszejko-Sochacki
-- List 4

import Data.List
import Control.Monad

-- task 1
data BTree a = BNode (BTree a) a (BTree a) | BLeaf

flatten :: BTree a -> [a]
flatten = flatten_aux []
  where
    flatten_aux :: [a] -> BTree a -> [a]
    flatten_aux acc (BNode l v r) = flatten_aux (v : flatten_aux acc r) l
    flatten_aux acc BLeaf         = acc

qsort :: Ord a => [a] -> [a]
qsort = qsort_aux []
  where
    qsort_aux :: Ord a => [a] -> [a] -> [a]
    qsort_aux acc (x:xs) = qsort_aux (x : qsort_aux acc [y | y <- xs, y >= x]) [y | y <- xs, y < x]
    qsort_aux acc []     = acc

-- task 5
class Queue q where
  emptyQ :: q a

  isEmptyQ :: q a -> Bool

  put :: a -> q a -> q a

  get :: q a -> (a, q a)
  get q = (top q, pop q)

  top :: q a -> a
  top = fst . get

  pop :: q a -> q a
  pop = snd . get

data SimpleQueue a = SimpleQueue { front :: [a], rear :: [a] }

instance Queue SimpleQueue where
  emptyQ   = SimpleQueue { front = [], rear = [] }

  isEmptyQ = null . front

  put x (SimpleQueue front rear) = SimpleQueue { front = front, rear = x : rear }

  top (SimpleQueue [] _)    = error "Top of empty queue"
  top (SimpleQueue front _) = head front

  pop (SimpleQueue (x:[]) rear) = SimpleQueue { front = reverse rear, rear = [] }
  pop (SimpleQueue (x:xs) rear) = SimpleQueue { front = xs, rear = rear }
  pop (SimpleQueue [] _)        = error "Pop of empty queue"

-- task 6
primes :: [Integer]
primes = 2 : [ p | p <- [3..], and [ p `mod` q /= 0 | q <- takeWhile ((p >=) . join (*)) primes]]

-- task 7
fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

-- task 8
(<+>) :: Ord a => [a] -> [a] -> [a]
xs@(x:xs') <+> ys@(y:ys') = case x `compare` y of
  LT -> x : xs' <+> ys
  EQ -> x : xs' <+> ys'
  GT -> y : xs <+> ys'

d235 :: [Integer]
d235 = 1 : [2,4..] <+> [3,6..] <+> [5,10..]

-- task 9
btreeBinNums :: BTree Int
btreeBinNums = BNode btreeBinNumsZero 1 btreeBinNums
  where
    btreeBinNumsZero :: BTree Int
    btreeBinNumsZero = BNode btreeBinNumsZero 0 btreeBinNums

-- task 10
btreeIntCycle :: BTree Int
btreeIntCycle = BNode btreeIntCycle 1 btreeIntCycle

data RoseTree a = RNode a [RoseTree a]

roseTreeCycle :: RoseTree Int
roseTreeCycle = RNode 1 roseTreesCycle
  where
    roseTreesCycle :: [RoseTree Int]
    roseTreesCycle = roseTreeCycle : roseTreesCycle

-- task 12
data Cyclist a = Elem (Cyclist a) a (Cyclist a)

fromList :: [a] -> Cyclist a
fromList xs'@(x:xs) = Elem (fromList (last xs' : init xs')) x (fromList (xs ++ [x]))
fromList []         = error "Cannot cycle empty list"

forward :: Cyclist a -> Cyclist a
forward (Elem _ _ next) = next

backward :: Cyclist a -> Cyclist a
backward (Elem prev _ _) = prev

label :: Cyclist a -> a
label (Elem _ v _) = v

-- task 13
enumInts :: Cyclist Integer
enumInts = enumAux 0
  where
    enumAux :: Integer -> Cyclist Integer
    enumAux n = Elem (enumAux $ n - 1) n (enumAux $ n + 1)
