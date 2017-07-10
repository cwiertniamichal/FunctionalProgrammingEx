
qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where leftPart xs = filter (<= x) xs
          rightPart xs = filter (> x) xs

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort (x:[]) = [x]
mSort xs = merge (mSort (leftPart xs)) (mSort (rightPart xs))
    where leftPart xs = take (length xs `div` 2) xs
          rightPart xs = drop (length xs `div` 2) xs
          merge [] ys = ys
          merge xs [] = xs
          merge (x:xs) (y:ys) = if x <= y 
            then x : merge xs (y:ys)
            else y : merge (x:xs) ys  

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:[]) = [x]
iSort (x:xs) = insert x (iSort xs)
    where insert x [] = [x]
          insert x (y:xs) 
                | y >= x  = x : y : xs  
                | otherwise = y : insert x xs

concat' :: [[a]] -> [a]
concat' xs = [y | x <- xs, y <- x]

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = x ++ concat'' xs

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x:xs) | x <= head xs = isSorted xs
                | otherwise = False   

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] 

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' (x:[]) = ([fst x], [snd x])
unzip' (x:xs) = (fst x : fst (unzipped xs), snd x : snd (unzipped xs) ) 
    where unzipped xs = unzip' xs 

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' [] _ _ = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs

subList :: Eq a => [a] -> [a] -> Bool 
subList xs ys = lol xs ys []
    where lol (x:xs) (y:ys) [] = if x == y then lol xs ys [x] || subList (x:xs) ys
                               else subList (x:xs) ys
          lol (x:xs) (y:ys) (z:zs) = if x == y then lol xs ys ((z:zs)++[x]) 
                                   else  False
          lol [] [] _ = True
          lol [] ys _ = True
          lol xs [] _ = False