-- Aleksander Czeszejko-Sochacki
-- List 7

{-# LANGUAGE Rank2Types #-}

-- task 6
newtype Church = Church (forall a. (a -> a) -> (a -> a))

zeroChurch :: Church
zeroChurch = Church $ \ f x -> x

oneChurch :: Church
oneChurch = Church $ \ f x -> f x 

fromChurch :: Church -> Integer
fromChurch (Church g) = g (+1) 0

toChurch :: Integer -> Church
toChurch n = Church $ aux n
  where
    aux 0 = \ f x -> x
    aux n = \ f x -> f $ aux (n - 1) f x

instance Eq Church where
  church1 == church2 = fromChurch church1 == fromChurch church2

instance Ord Church where
  church1 <= church2 = fromChurch church1 <= fromChurch church2

instance Show Church where
  show = show . fromChurch

instance Num Church where
  (Church n) + (Church m) = Church $ \ f -> n f . m f
  (Church n) * (Church m) = Church $ n . m
  church1 - church2 = toChurch $ abs (fromChurch church1 - fromChurch church2)
  abs = id
  fromInteger = toChurch
  signum church = if fromChurch church > 0 then oneChurch else zeroChurch

-- task 7
newtype CList x = CList (forall a. (x -> a -> a) -> a -> a)

cempty :: CList x
cempty = CList $ \ f y -> y

ccons :: x -> CList x -> CList x
ccons x (CList xs) = CList $ \ f y -> f x (xs f y)

cappend :: CList x -> CList x -> CList x
cappend c1 c2 =
  let xs = ctoList c1
  in foldr ccons c2 xs

cfromList :: [x] -> CList x
cfromList []     = cempty
cfromList (x:xs) = ccons x $ cfromList xs

ctoList :: CList x -> [x]
ctoList (CList xs) = xs (:) []
