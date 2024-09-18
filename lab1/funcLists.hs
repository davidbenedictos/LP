myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs)
    |x >= (myMaximum xs) = x
    |otherwise = myMaximum xs

average :: [Int] -> Float
average x = fromIntegral (sumOf x) / fromIntegral (myLength x)
 

sumOf :: [Int] -> Int
sumOf [] = 0
sumOf (x:xs) = x + (sumOf xs)

buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome x = (reversedList x) ++ x

reversedList :: [Int] -> [Int]
reversedList [] = []
reversedList [x] = [x]
reversedList (x:xs) = (reversedList xs) ++ [x]

remove :: [Int] -> [Int] -> [Int]
remove [] y = []
remove x [] = x
remove x [y] = removeYfromX x y
remove x (y:ys) = remove (removeYfromX x y) ys


removeYfromX :: [Int] -> Int -> [Int]
removeYfromX [x] y
    |y == x = []
    |otherwise = [x]
removeYfromX (x:xs) y
    |y == x = removeYfromX xs y
    |otherwise = [x] ++ (removeYfromX xs y)

flatten :: [[Int]] -> [Int] 
flatten [] = []
flatten (x:xs) = x ++ (flatten xs)

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([], [])
oddsNevens (x:xs) 
    |(mod x 2) /= 0 = (x:a, b)
    |otherwise = (a, x:b)
    where
        (a, b) = oddsNevens xs


primeDivisors :: Int -> [Int]
primeDivisors x = primeToX x 2 

primeToX :: Int -> Int -> [Int]
primeToX x a
    | a > (div x 2) = []
    | mod x a == 0 && isPrime a = a:lista
    | otherwise = lista
    where
        lista = primeToX x (a + 1)


isPrime :: Int -> Bool
isPrime n
  |n < 2 = False
  |otherwise = isPrimeRec n (n-1)

isPrimeRec :: Int -> Int -> Bool
isPrimeRec n p
    |p == 1 = True
    |mod n p == 0 = False
    |otherwise = isPrimeRec n (p -1)
