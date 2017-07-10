import Data.Char

doubleElems [] = []
doubleElems (x:xs) = 2 * x : doubleElems xs

sqrElems [] = []
sqrElems (x:xs) = x * x : sqrElems xs

lowerCase [] = []
lowerCase (x:xs) = toLower x : lowerCase xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

doubleElemsUsingMap = map' (\x -> 2 * x) 
sqrElemsUsingMap = map' (\x -> x * x)
lowerCaseUsingMap = map' (\x -> toLower x)

doubleElemsUsingLC xs = [2 * x | x <- xs]
sqrElemsUsingLC xs = [x * x | x <- xs]
lowerCaseUsingLC xs = [toLower x | x <- xs]


evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x = map (\y -> y x)
