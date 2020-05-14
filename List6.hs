-- Aleksander Czeszejko-Sochacki
-- List 6

import Prelude hiding (reverse, tail, zip)

-- task 1
natPairs :: [(Integer, Integer)]
natPairs = concatMap (\x -> zip [0..x] [x,(x-1)..0]) [0..]

(><) :: [a] -> [b] -> [(a, b)]
(><) xs ys = aux [] [] xs ys
  where
    aux :: [a] -> [b] -> [a] -> [b] -> [(a, b)]
    aux _ _ [] []               = []
    aux [] [] _ []              = []
    aux [] [] [] _              = []
    aux (y:ys) acc2 (x:xs) []   =
      let newAcc1 = ys ++ [x]
      in (zip newAcc1 acc2) ++ (aux newAcc1 acc2 xs [])
    aux acc1 acc2 [] (y:ys)     =
      let newAcc2 =  y : (init acc2)
      in (zip acc1 newAcc2) ++ (aux acc1 newAcc2 [] ys)
    aux acc1 acc2 (x:xs) (y:ys) =
      let
        newAcc1 = acc1 ++ [x]
        newAcc2 = y : acc2 
      in (zip newAcc1 newAcc2) ++ (aux newAcc1 newAcc2 xs ys)

-- task 6
tail :: [a] -> [a]
tail xs = foldr (\ y ys -> if (length ys) == (length xs) - 1 then ys else y : ys) [] xs

reverse :: [a] -> [a]
reverse = foldr (flip (++) . return) []

zip :: [a] -> [b] -> [(a, b)]
zip xs ys = foldr f (const []) xs ys
  where
    f x r []     = []
    f x r (y:ys) = (x,y) : r ys

-- task 7
data RAList a = RAZero (RAList (a,a)) | RAOne a (RAList (a,a)) | RANil

class ListView t where
  toList :: t a -> [a]
  cons :: a -> t a -> t a
  nil :: t a

instance ListView RAList where
  toList RANil         = []
  toList (RAZero xs)   = foldr (\ (x, y) xs -> x : y : xs) [] $ toList xs
  toList (RAOne xs ys) = xs : (foldr (\ (x, y) xs -> x : y : xs) [] $ toList ys)

  cons x RANil        = RAOne x RANil
  cons x (RAZero xs)  = RAOne x xs
  cons x (RAOne y xs) = RAZero $ cons (x, y) xs

  nil = RANil