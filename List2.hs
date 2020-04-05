-- Aleksander Czeszejko-Sochacki
-- List 2
-- task 1
{-# LANGUAGE ParallelListComp, TupleSections #-}
import Data.Bool
import Control.Monad

subseqC :: [a] -> [[a]]
subseqC [] = [[]]
subseqC (x:xs) = [el | ys <- subseqC xs, el <- [(x:ys), ys]]

splitC :: [a] -> [([a], [a])]
splitC xs = [(pref, suff) | i <- [0..(length xs)], pref <- [take i xs], suff <- [drop i xs]]

ipermC :: [a] -> [[a]]
ipermC [] = [[]]
ipermC (x:xs) = [(ys ++ (x : zs)) | subPerm <- ipermC xs, (ys, zs) <- splitC subPerm]

spermC :: [a] -> [[a]]
spermC [] = [[]]
spermC xs = [el : perm | (el, rest) <- select xs, perm <- spermC rest]
  where
    select :: [a] -> [(a, [a])]
    select xs = [(hd, pref ++ suff) | (pref, (hd : suff)) <- splitC xs]

qsortC :: Ord a => [a] -> [a]
qsortC [] = []
qsortC (x:xs) = [el | ys <- [qsortC [y | y <- xs, y <= x]], zs <- [qsortC [z | z <- xs, z > x]], el <- ys ++ (x : zs)]

zipC :: [a] -> [b] -> [(a, b)]
zipC xs ys = [(x, y) | x <- xs | y <- ys]


-- task 2
zipP = \xs -> ap (flip bool [] . (ap ((:) . ((head xs ,) . head)) ((zipP . tail) xs . tail))) (((. null) . (&&) . null) xs)

ipermF :: [a] -> [[a]]
ipermF [] = [[]]
ipermF (x:xs) = concatMap insert (ipermF xs)
  where
    insert = \ys -> flip bool [[x]] (
      ((:) . (:) x)
      ys
      ( (map (((:) . head) ys)) ((insert . tail) ys) )) (null ys)

qsortP :: Ord a => [a] -> [a]
qsortP = ap (flip bool ([]) .
  ap ((++) . qsortP . ap (filter . flip (<=) . head) tail)
  (ap ((++) . return . head) (qsortP . ap (filter . flip (>) . head) tail))) null


-- task 3
data Combinator = S | K | Combinator :$ Combinator
infixl :$

instance Show Combinator where
  show S = show "S"
  show K = show "K"
  show (K :$ K) = show K ++ show K
  show (K :$ S) = show K ++ show S
  show (S :$ K) = show S ++ show K
  show (S :$ r) = show S ++ "(" ++ show r ++ ")"
  show (l :$ r) = show l ++ "(" ++ show r ++ ")"


-- task 5
data BST a = NodeBST (BST a) a (BST a) | EmptyBST deriving Show

searchBST :: Ord a => a -> BST a -> Maybe a
searchBST x (NodeBST l v r) = case (x `compare` v) of
  LT -> searchBST x l
  EQ -> Just v
  GT -> searchBST x r
searchBST _ EmptyBST = Nothing

insertBST :: Ord a => a -> BST a -> BST a
insertBST x bst@(NodeBST l v r) = case x `compare` v of
  LT -> NodeBST (insertBST x l) v r
  EQ -> bst
  GT -> NodeBST l v (insertBST x r)
insertBST x EmptyBST = NodeBST EmptyBST x EmptyBST


-- task 6
findMinBST :: Ord a => BST a -> a
findMinBST (NodeBST EmptyBST v _) = v
findMinBST (NodeBST l v _) = findMinBST l
findMinBST EmptyBST = error "Empty BST has not a minimal element"

deleteMinBST :: Ord a => BST a -> (BST a, a)
deleteMinBST bst = (aux bst, findMinBST bst)
  where
    aux :: Ord a => BST a -> BST a
    aux (NodeBST EmptyBST v r) = r
    aux (NodeBST l v r) = NodeBST (aux l) v r
    aux EmptyBST = error "Empty BST has not a minimal element"

deleteBST :: Ord a => a -> BST a -> BST a
deleteBST x bst@(NodeBST l v r) = case x `compare` v of
  LT -> NodeBST (deleteBST x l) v r
  GT -> NodeBST l v (deleteBST x r)
  EQ -> case (l, r) of
    (EmptyBST, EmptyBST) -> EmptyBST
    (EmptyBST, r)     -> r
    (l, EmptyBST)     -> l
    otherwise      -> case r of
      NodeBST EmptyBST v2 r2 -> NodeBST l v2 r2
      NodeBST l2 v2 EmptyBST  -> NodeBST l v2 l2
      NodeBST l2 v2 r      ->
        let (delMinBST, minBST) = deleteMinBST r
        in NodeBST l minBST delMinBST 
deleteBST x EmptyBST = error "Cannot delete from empty BST"


-- task 7
data Tree23 a = Node2 (Tree23 a) a (Tree23 a)
  | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
  | Empty23

search23 :: Ord a => a -> Tree23 a -> Maybe a
search23 x (Node2 l v r) = case x `compare` v of
  LT -> search23 x l
  EQ -> Just v
  GT -> search23 x r
search23 x (Node3 l v1 m v2 r) = case (x `compare` v1, x `compare` v2) of
  (LT, _)  -> search23 x l
  (EQ, _)  -> Just v1
  (GT, LT) -> search23 x m
  (_, EQ)  -> Just v2
  (_, GT)  -> search23 x r
search23 _ Empty23 = Nothing
