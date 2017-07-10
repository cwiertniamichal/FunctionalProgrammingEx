sumWith g [] = 0
sumWith g (x:xs) = g x + sumWith g xs

prodWith g [] = 1
prodWith g (x:xs) = g x * prodWith g xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
    where
      go acc g [] = acc
      go acc g (x:xs) = go (acc + g x) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
    where
      go acc g [] = acc
      go acc g (x:xs) = go (acc * g x) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z [] = z
foldr' f z (x:xs) = f x (foldr f z xs)

sumWith'' g  = foldr' (\x acc -> g x + acc) 0
prodWith'' g = foldr' (\x acc -> g x * acc) 1

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs  

sumWith''' g = foldl' (\acc x -> g x + acc) 0
prodWith''' g = foldl' (\acc x -> g x * acc) 1 

mapUsingFoldr :: (a -> b) -> [a] -> [b]
mapUsingFoldr _ [] = []
mapUsingFoldr f xs = foldr (\x acc -> f x : acc) [] xs 

mapUsingFoldl :: (a -> b) -> [a] -> [b]
mapUsingFoldl _ [] = []
mapUsingFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

filterUsingFoldr :: (a -> Bool) -> [a] -> [a]
filterUsingFoldr _ [] = []
filterUsingFoldr f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

foldlFilter :: (a -> Bool) -> [a] -> [a]
foldlFilter _ [] = []
foldlFilter f xs = foldl (\acc x -> if f x then acc ++ [x] else acc) [] xs

foldlUsingFoldr :: (b -> a -> b) -> b -> [a] -> b
foldlUsingFoldr f z [] = z
foldlUsingFoldr f z xs = foldr (\x acc -> f acc x) z (reverse xs)

foldrUsingFoldl :: (a -> b -> b) -> b -> [a] -> b
foldrUsingFoldl _ z [] = z
foldrUsingFoldl f z xs = foldl (\acc x -> f x acc) z (reverse xs)