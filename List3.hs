-- Aleksander Czeszejko-Sochacki
-- List 3
{-# LANGUAGE ViewPatterns, FlexibleInstances, UndecidableInstances #-}

-- task 1
data BTree t a = Node (t a) a (t a) | Leaf

class BT t where
  toTree :: t a -> BTree t a

treeSize :: BT t => t a -> Int
treeSize (toTree -> Node l _ r) = treeSize l + treeSize r + 1
treeSize (toTree -> Leaf)       = 0

treeLabels :: BT t => t a -> [a]
treeLabels = flip aux [] where
  aux :: BT t => t a -> [a] -> [a] 
  aux (toTree -> Node l v r) acc = aux l (v : aux r acc)
  aux (toTree -> Leaf) acc       = acc

treeFold :: BT t => (b -> a -> b -> b) -> b -> t a -> b
treeFold f acc (toTree -> Node l v r) = f (treeFold f acc l) v (treeFold f acc r)
treeFold _ acc (toTree -> Leaf)       = acc


-- task 2
data UTree a = UNode (UTree a) a (UTree a) | ULeaf

instance BT UTree where
  toTree ULeaf = Leaf
  toTree (UNode l x r) = Node l x r

newtype Unbalanced a = Unbalanced { fromUnbalanced :: BTree Unbalanced a }

searchBT :: (Ord a, BT t) => a -> t a -> Maybe a
searchBT x (toTree -> Node l v r) = case x `compare` v of
  LT -> searchBT x l
  EQ -> Just v
  GT -> searchBT x r
searchBT x (toTree -> Leaf) = Nothing

toUTree :: BT t => t a -> UTree a
toUTree (toTree -> Node l v r) = UNode (toUTree l) v (toUTree r)
toUTree (toTree -> Leaf)       = ULeaf

toUnbalanced :: BT t => t a -> Unbalanced a
toUnbalanced (toTree -> Node l v r) = Unbalanced { fromUnbalanced = Node (toUnbalanced l) v (toUnbalanced r) }
toUnbalanced (toTree -> Leaf)       = Unbalanced { fromUnbalanced = Leaf }


-- task 3
instance (BT t, Show a) => Show (t a) where
  show (toTree -> Node (toTree -> Leaf) v (toTree -> Leaf)) = "- " ++ show v ++ " -"
  show (toTree -> Leaf) = "-"
  show (toTree -> Node l v r) = aux l ++ " " ++ show v ++ " " ++ show r
    where
      aux (toTree -> Node l v r) = "(" ++ aux l ++ " " ++ show v ++ " " ++ aux r ++ ")"
      aux (toTree -> Leaf) = "-"


-- task 6
class BT t => BST t where
  node :: t a -> a -> t a -> t a
  leaf :: t a

class Set s where
  empty :: s a
  search :: Ord a => a -> s a -> Maybe a
  insert :: Ord a => a -> s a -> s a
  delMax :: Ord a => s a -> Maybe (a, s a)
  delete :: Ord a => a -> s a -> s a

instance BST s => Set s where
  empty = leaf
  search = searchBT

  insert x (toTree -> Node l v r) = case x `compare` v of
    LT -> node (insert x l) v r
    EQ -> node l v r
    GT -> node l v (insert x r)
  insert x (toTree -> Leaf) = node leaf x leaf

  delMax (toTree -> Node l v (toTree -> Leaf)) = Just (v, node l v leaf)
  delMax (toTree -> Node l v r) = delMax r >>= \(max, deleted) -> Just (max, node l v deleted)
  delMax (toTree -> Leaf) = Nothing

  delete x (toTree -> Node l v r) = case x `compare` v of
    LT -> node (delete x l) v r
    EQ -> case delMax l of
      Just (max, deleted) -> node deleted max r
      Nothing             -> r
    GT -> node l v (delete x r)
  delete x (toTree -> Leaf) = leaf


-- task 7
data WBTree a = WBNode (WBTree a) a Int (WBTree a) | WBLeaf

wbtTreeSize :: WBTree a -> Int
wbtTreeSize (WBNode _ _ n _) = n
wbtTreeSize WBLeaf           = 0

wbtNodeSize :: WBTree a -> WBTree a -> Int
wbtNodeSize l r = wbtTreeSize l + wbtTreeSize r + 1

wbtTreeSizeLeft :: WBTree a -> Int
wbtTreeSizeLeft (WBNode l _ _ _) = wbtTreeSize l
wbtTreeSizeLeft WBLeaf           = 0

wbtTreeSizeRight :: WBTree a -> Int
wbtTreeSizeRight (WBNode _ _ _ r) = wbtTreeSize r
wbtTreeSizeRight WBLeaf           = 0

rotateLSingle :: WBTree a -> WBTree a
rotateLSingle (WBNode l v n (WBNode rl rv rn rr)) = WBNode (WBNode l v (wbtNodeSize l rl) rl) rv n rr

rotateRSingle :: WBTree a -> WBTree a
rotateRSingle (WBNode (WBNode lr lv ln ll) v n r) = WBNode ll lv n (WBNode lr v (wbtNodeSize lr r) r)

rotateLDouble :: WBTree a -> WBTree a
rotateLDouble (WBNode l v n (WBNode (WBNode rll rlv rln rlr) rv rn rr)) =
  WBNode (WBNode l v (wbtNodeSize l rll) rll) rlv n (WBNode rlr rv (wbtNodeSize rlr rr) rr) 

rotateRDouble :: WBTree a -> WBTree a
rotateRDouble (WBNode (WBNode ll lv ln (WBNode lrl lrv lrn lrr)) v n r) =
  WBNode (WBNode lrl lv (wbtNodeSize lrl ll) ll) lrv n (WBNode r v (wbtNodeSize r lrr) lrr)

omega = 5

instance BT WBTree where
  toTree (WBNode l v _ r) = Node l v r
  toTree WBLeaf           = Leaf

instance BST WBTree where
  node l v r = balanceWBTree $ WBNode l v 0 r  -- size will be computed after balance
    where
      balanceWBTree (WBNode l v _ r)
        | treeSize l > omega * treeSize r && wbtTreeSizeLeft l > wbtTreeSizeRight r = rotateRDouble balancedSubtrees
        | treeSize l > omega * treeSize r && wbtTreeSizeLeft l <= wbtTreeSizeRight r = rotateRSingle balancedSubtrees
        | treeSize r > omega * treeSize l && wbtTreeSizeLeft r > wbtTreeSizeRight r = rotateLDouble balancedSubtrees
        | treeSize r > omega * treeSize l && wbtTreeSizeLeft r <= wbtTreeSizeRight r = rotateLSingle balancedSubtrees
        where
          balancedSubtrees = WBNode (balanceWBTree l) v (wbtNodeSize l r) (balanceWBTree r)

      balanceWBTree WBLeaf = WBLeaf
  
  leaf = WBLeaf