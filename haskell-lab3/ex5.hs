
import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

sortDescPointFree :: Ord a => [a] -> [a]
sortDescPointFree = reverse . sort

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt _ _ [] = True
are2FunsEqAt f g (x:xs) = (f x == g x) && (are2FunsEqAt f g xs) 

infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g  

composeFunList :: [a -> a] -> (a -> a)
composeFunList [] = id
composeFunList (f:[]) = f
composeFunList (f:fs) = f . composeFunList fs 
