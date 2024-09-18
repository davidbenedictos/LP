myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ ini [] = ini
myFoldl f ini (x:xs) = myFoldl f (f ini x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ ini [] = ini
myFoldr f ini (x:xs) = f x (myFoldr f ini xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x] ++ (myIterate f (f x)) 

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil cond f a
    |cond a == True = a
    |otherwise = myUntil cond f (f a)

myMap :: (a -> b) -> [a] -> [b]
myMap f a= myFoldl (\acc x ->acc ++ [f x]) [] a

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f a = myFoldl (\acc x -> if f x then (acc ++ [x]) else acc) [] a

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = foldr (\x acc -> f x && acc) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = [(x, y)] ++ (myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b= myFoldl (\acc (x,y) -> acc ++ [f x y]) [] (myZip a b) 