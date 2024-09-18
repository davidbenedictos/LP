insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (x:xs) a
    |x < a = [x] ++ (insert xs a)
    |otherwise = [a] ++ [x] ++ xs 

isort :: [Int] -> [Int]
isort [] = []
isort [x] = [x]
isort x = isortRec x []

isortRec :: [Int] -> [Int] -> [Int]
isortRec (x:xs) s
    |xs == [] = insert s x 
    |otherwise = isortRec xs (insert s x)

remove :: [Int] -> Int -> [Int]
remove [] x = []
remove (x:xs) i
    | x == i = xs
    | otherwise =  [x] ++ remove xs i

ssort :: [Int] -> [Int]
ssort [] = []
ssort lista = ssortRec lista []

ssortRec :: [Int] -> [Int] -> [Int] 
ssortRec [] res = res
ssortRec ini res = ssortRec quitamosMax ponemosMax
    where
        ponemosMax = maximum(ini):res
        quitamosMax = remove ini (maximum(ini))
        

merge :: [Int] -> [Int] -> [Int]
merge [] b = b
merge a [] = a
merge (pA:a) (pB:b)
    |pA < pB = pA:(merge a (pB:b))
    |otherwise = pB:(merge (pA:a) b)

msort :: [Int] -> [Int] 
msort [] = []
msort [x] = [x]
msort l = merge (msort iz) (msort der)
    where 
    (iz, der) = (take n l, drop n l)
    n = div (length l) 2

qsort :: [Int] -> [Int]
qsort []     = []
qsort (p:xs) = (qsort menors) ++ [p] ++ (qsort majors)
    where
        menors = [x | x <- xs, x <  p]
        majors = [x | x <- xs, x >= p]


genQsort :: Ord a => [a] -> [a]
genQsort []     = []
genQsort (p:xs) = (genQsort menors) ++ [p] ++ (genQsort majors)
    where
        menors = [x | x <- xs, x <  p]
        majors = [x | x <- xs, x >= p]    