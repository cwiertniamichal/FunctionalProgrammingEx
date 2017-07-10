import Data.List
fib :: (Num a, Eq a) => a -> a
fib n =
    if n == 0 || n == 1 then n 
    else fib (n - 2) + fib (n - 1)


fib2 :: Int -> Int
fib2 n = fibs !! n

fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
    where loop acc []  = acc
          loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

prod' :: Num a => [a] -> a
prod' [] = 0
prod' (x: []) = x
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' x xs = 
    if xs == [] 
    then False
    else if x == head xs 
        then True
        else elem' x (tail xs)

elem'' :: Eq a => a -> [a] -> Bool
elem'' x [] = False
elem'' x (y:xs) = if x == y 
    then True
    else elem'' x xs


doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:[]) = [2*x]
doubleAll (x:xs) = 2*x : doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:[]) = [x*x]
squareAll (x:xs) = x*x : squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = if even x 
    then x : selectEven xs
    else selectEven xs 

average :: Fractional a => [a] -> a
average [] = 0
average (x:[]) = x
average (x:xs) = licz / mian
    where licz = x + average xs * genericLength xs 
          mian =  genericLength xs + 1

geometricAverage :: Floating a => [a] -> a
geometricAverage [] = 0
geometricAverage (x:[]) = x
geometricAverage (x:xs) = (x * geometricAverage xs ** genericLength xs) ** (1/(genericLength xs + 1))

bothAverages :: Floating a => [a] -> (a, a)
bothAverages xs = (average' xs, geometricAverage' xs)
    where average' [] = 0
          average' (x:[]) = x
          average' (x:xs) = (x + average' xs * genericLength xs) / (genericLength xs + 1)
          geometricAverage' [] = 0
          geometricAverage' (x:[]) = x
          geometricAverage' (x:xs) = (x * geometricAverage' xs ** genericLength xs) ** (1/(genericLength xs + 1))
        
bothAverages' :: Floating a => [a] -> (a, a)
bothAverages' xs = (average xs, geometricAverage xs)