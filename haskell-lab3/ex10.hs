

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = all (\(x,y) -> x <= y) (zip xs (drop 1 xs))

everySecond :: [t] -> [t]
everySecond xs = map fst (filter (\(x,y) -> y `mod` 2 == 0) (zip xs [0..(length xs)]))

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs 

unzip3' :: [(a, b, c)] -> ([a], [b], [c])
unzip3' [] = ([], [], [])
unzip3' ((x, y, z) : xyzs) = (x : xs, y : ys, z : zs)
    where (xs, ys, zs) = unzip3' xyzs 

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc xs = all (\(x,y) -> x >= y) (zip xs (drop 1 xs))
