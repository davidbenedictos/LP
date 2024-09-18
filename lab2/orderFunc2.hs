flatten :: [[Int]] -> [Int]
flatten a = foldr (++) [] a

--myLengthInt :: [Int] -> Int
--myLengthInt a = foldl (+) 0 ( map (const 1) a)

myLength :: String -> Int
myLength [] = 0
myLength a = last $ zipWith(\x y -> y) a $ iterate (+1) 1

myReverse :: [Int] -> [Int] 
myReverse a = foldr (girar) [] a

girar :: Int -> [Int] -> [Int]
girar x y = y ++ [x]

countIn :: [[Int]] -> Int -> [Int]
countIn a x = map (contar x) a
    where
        contar x vec = length ( filter (== x) vec)

firstWord :: String -> String
firstWord a = takeWhile (/= ' ') $ dropWhile (== ' ') a