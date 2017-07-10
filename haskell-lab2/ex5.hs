
isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

isPrime' :: Int -> Bool
isPrime' n = if n `elem` [x | x <- takeWhile (<= n) primes]
    then True
    else False

primes :: [Int]
primes = eratoSieve [2..]
 where
    eratoSieve :: [Int] -> [Int]
    eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

countPrimes :: Int -> Int -> Int
countPrimes x y = sum [1 | z <- takeWhile (>x) (takeWhile (<y) primes)]

allEqual :: Eq a => [a] -> Bool
allEqual xs = if xs == [] 
    then True
    else
        [x | x <- tail xs, x /= head xs] == []