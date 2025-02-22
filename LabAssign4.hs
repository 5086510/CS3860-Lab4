-- Nemanja Blagojevic
-- CS3860
-- LabAssign4.hs

import Data.Char (chr, ord, isLetter, isUpper, isLower)

-- Task 1: Transpose a matrix
transp :: [[a]] -> [[a]]
transp [] = []
transp ([]:_) = []
transp x = map head x : transp (map tail x)

-- Task 2: Matrix Multiplication
matmult :: Num a => [[a]] -> [[a]] -> [[a]]
matmult a b = [[ sum $ zipWith (*) row col | col <- transp b ] | row <- a]

-- Task 3: Pascal's Triangle
pascal :: [Int] -> [Int]
pascal row = zipWith (+) (0:row) (row++[0])

triangleR :: Int -> [Int]
triangleR 0 = [1]
triangleR n = pascal (triangleR (n-1))

pascalTriangle :: Int -> [[Int]]
pascalTriangle n = [triangleR i | i <- [0..n]]

-- Task 4

shiftChar :: Char -> Int -> Char
shiftChar c n
  | isUpper c = chr (((ord c - ord 'A' + n) `mod` 26) + ord 'A')
  | isLower c = chr (((ord c - ord 'a' + n) `mod` 26) + ord 'a')
  | c == ' '  = 'c'  -- Change spaces to 'z'
  | otherwise = c

shiftCharBack :: Char -> Int -> Char
shiftCharBack c n
  | c == 'c'  = ' '  -- Decrypt 'z' back to space
  | isUpper c = chr (((ord c - ord 'A' - n) `mod` 26) + ord 'A')
  | isLower c = chr (((ord c - ord 'a' - n) `mod` 26) + ord 'a')
  | otherwise = c

cipher :: String -> Int -> String
cipher text n = map (`shiftChar` n) text

decipher :: String -> Int -> String
decipher text n = map (`shiftCharBack` n) text

