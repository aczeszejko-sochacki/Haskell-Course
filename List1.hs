-- Aleksander Czeszejko-Sochacki
-- Haskell Course, List 1

-- Task 1
import Prelude hiding (concat, and, all)
import Data.Char

intercalate :: [a] -> [[a]] -> [a]
intercalate xs [[]]     = []
intercalate xs (ys:yss) = ys ++ foldl (\acc -> \zs -> xs ++ zs ++ acc) [] (reverse yss)

transpose :: [[a]] -> [[a]]
tranpose [] = []
transpose xs = init $ transposeAux xs
  where
    transposeAux [] = []
    transposeAux xs =
      let filtered = filter (not . null) xs
      in (map head filtered) : transposeAux (map tail filtered)

concat :: [[a]] -> [a]
concat = foldl (++) []

and :: [Bool] -> Bool
and = foldl (&&) True

all :: (a -> Bool) -> [a] -> Bool
all f = and . (map f) 

maximum :: [Integer] -> Integer
maximum []     = undefined
maximum (x:[]) = x
maximum (x:xs) = foldl (max) x xs

-- Task 2
newtype Vector a = Vector { fromVector :: [a] }

scaleV :: Num a => a -> Vector a -> Vector a
scaleV r (Vector v) = Vector { fromVector = map (*r) v }

norm :: Floating a => Vector a -> a
norm (Vector v) = sqrt $ sum $ map (^2) v

scalarProd :: Num a => Vector a -> Vector a -> a
scalarProd (Vector v) (Vector u) =
  if (length v) == (length u)
    then sum $ zipWith (*) v u
    else error "Different lenghts of vectors"
    
sumV :: Num a => Vector a -> Vector a -> Vector a
sumV (Vector v) (Vector u) =
  if (length v) == (length u)
    then Vector { fromVector = zipWith (+) v u }
    else error "Different lenghts of vectors"

-- Task 3
newtype Matrix a = Matrix { fromMatrix :: [[a]] }

-- Matrix validation
isRectMatrix :: Num a => Matrix a  ->  Bool
isRectMatrix (Matrix (xs:xss)) = all ((== (length xs)) . length) xss

isSquareMatrix :: Num a => Matrix a -> Bool
isSquareMatrix (Matrix [[]]) = True
isSquareMatrix m@(Matrix (xs:xss)) = isRectMatrix m && (length xss) + 1 == (length xs)

prodCorrectDims :: Num a => Matrix a -> Matrix a -> Bool
prodCorrectDims m@(Matrix (xss)) n@(Matrix (ys:_)) = isRectMatrix m && isRectMatrix n && (length xss) == (length ys)

sameSizeMatrices :: [[a]] -> [[a]] -> Bool
sameSizeMatrices xss yss =
  (length xss) == (length yss) && and (map (\(xs,ys) -> (length xs) == (length ys)) $ zip xss yss)

-- Operations
sumM :: Num a => Matrix a -> Matrix a -> Matrix a
sumM (Matrix m) (Matrix n) =
  if sameSizeMatrices m n
    then Matrix { fromMatrix = zipWith (zipWith (+)) m n }
    else error "Matrices should have the same coresponding dimensions"

prodM :: Num a => Matrix a -> Matrix a -> Matrix a
prodM m@(Matrix xss) n@(Matrix yss) =
  if prodCorrectDims m n
    then Matrix { fromMatrix = [[sum $ zipWith (*) ms ns | ms <- (transpose xss) ] | ns <- yss] }
    else error "Wrong matrices sizes"

det :: Num a => Matrix a -> a
det m@(Matrix xss) =
  if isSquareMatrix m
    then detAux xss
    else error ""
      where
        detAux :: Num a => [[a]] -> a
        detAux [[x]] = x
        detAux m     =
          let dim = (length m)
          in sum [(-1)^(i + j) * (selectEl m i j) * detAux (minor m i j) | i <- [1..dim], j <- [1..dim]]

        selectEl :: Num a => [[a]] -> Int -> Int -> a
        selectEl m i j = (m !! (i - 1)) !! (j - 1)
        
        minor :: Num a => [[a]] -> Int -> Int -> [[a]]
        minor [[]] _ _ = error ("Cannot get minor from an empty array")
        minor m i j    = map (deleteNth (j - 1)) $ deleteNth (i - 1) m
        
        deleteNth :: Int -> [a] -> [a]
        deleteNth _ []     = []
        deleteNth i (x:xs)
          | i == 0    = xs
          | otherwise = x : deleteNth (i-1) xs

-- Task 4
isbn13_check :: String -> Bool
isbn13_check isbn = (length isbn) == 17 && (isbnInitVal $ init $ isbnDeleteDashes isbn) == (digitToInt $ last isbn)
  where
    isbnDeleteDashes :: String -> String
    isbnDeleteDashes = filter (/= '-')
    
    isbnInitVal :: String -> Int
    isbnInitVal xs =
      let weights = take 12 $ cycle [1,3]
          digits  = map digitToInt xs
      in 
        let weightedSumMod = (sum $ zipWith (*) weights digits) `mod` 10
        in
          if weightedSumMod == 0
            then 0
            else 10 - weightedSumMod
