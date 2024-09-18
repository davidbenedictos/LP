absValue :: Int -> Int
absValue n
    |n >= 0 = n
    |otherwise = -n


power :: Int -> Int -> Int
power _ 0 = 1
power n p = n * power n (p - 1)

isPrime :: Int -> Bool
isPrime n
  |n < 2 = False
  |otherwise = isPrimeRec n (n-1)

isPrimeRec :: Int -> Int -> Bool
isPrimeRec n p
    |p == 1 = True
    |mod n p == 0 = False
    |otherwise = isPrimeRec n (p -1)

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 1) + slowFib(n - 2) 

quickFib :: Int -> Int
quickFib 0 = 0
quickFib n = aFib n 0 0 1

aFib :: Int -> Int -> Int -> Int -> Int
aFib n r prev curr
    | r == n    = prev
    | otherwise = aFib n (r + 1) curr (prev + curr)
