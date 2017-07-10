sum' :: Num a => [a] -> a
sum' []  = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sumUsingSumWith = sumWith (\e -> e)
sumSqrUsingSumWith = sumWith (\e -> e^2)
sumCubeUsingSumWith = sumWith (\e -> e^3)
sumAbsUsingSumWith = sumWith (\e -> abs e)

listLengthUsingSumWith = sumWith (\e -> 1)

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith _ [] = 1
prodWith f (x:xs) = f x * prodWith f xs

prodUsingProdWith = prodWith (\e -> e)
prodSqrUsingProdWith = prodWith (\e -> e^2)
prodCubeUsingProdWith = prodWith (\e -> e^3)
prodAbsUsingProdWith = prodWith (\e -> abs e)

calcWith :: Num a => ((a -> a) -> [a] -> a) -> (a -> a) -> [a] -> a
calcWith op f xs =  op f xs

sumSqrtUsingSumWith = sumWith (\e -> sqrt(e))
prodSqrtUsingProdWith = prodWith (\e -> sqrt(e))