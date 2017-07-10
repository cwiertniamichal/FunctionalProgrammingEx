data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT val lt rt) = val + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a) deriving (Show)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT val lt rt) = val + sumBinTree lt + sumBinTree rt

data Expr a = Lit a |
              Add (Expr a) (Expr a) |
              Sub (Expr a) (Expr a) |
              Mul (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"  
show' (Sub e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Mul e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT _ lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)


insert :: Ord a => a -> BinTree a -> BinTree a
insert e EmptyBT = NodeBT e EmptyBT EmptyBT
insert e (NodeBT val lt rt)
    | val == e = (NodeBT val lt rt)
    | val < e = (NodeBT val lt (insert e rt))
    | val > e = (NodeBT val (insert e lt) rt)

list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST xs = foldr insert EmptyBT xs

flattenBTPre :: BinTree a -> [a]
flattenBTPre EmptyBT = []
flattenBTPre (NodeBT val lt rt) = val : (flattenBTPre lt) ++ (flattenBTPre rt)

flattenBTIn :: BinTree a -> [a]
flattenBTIn EmptyBT = []
flattenBTIn (NodeBT val lt rt) = (flattenBTIn lt) ++ [val] ++ (flattenBTIn rt)

flattenBTPost :: BinTree a -> [a]
flattenBTPost EmptyBT = []
flattenBTPost (NodeBT val lt rt) = (flattenBTPost lt) ++ (flattenBTPost rt) ++ [val]

occurs :: Eq a => a -> BinTree a -> Int
occurs _ EmptyBT = 0 
occurs n (NodeBT val lt rt) = case n == val of
    True -> 1 + occurs n lt + occurs n rt
    False -> occurs n lt + occurs n rt

elemOf :: Ord a => a -> BinTree a -> Bool
elemOf _ EmptyBT = False
elemOf n (NodeBT val lt rt) = case n == val of
    True -> True
    False -> (elemOf n lt) || (elemOf n rt)

reflect :: BinTree a -> BinTree a
reflect EmptyBT = EmptyBT
reflect (NodeBT val EmptyBT EmptyBT) = NodeBT val EmptyBT EmptyBT 
reflect (NodeBT val lt rt) =  NodeBT val (reflect rt) (reflect lt)

minElemOf :: Ord a => BinTree a -> a
minElemOf (NodeBT val EmptyBT EmptyBT) = val
minElemOf (NodeBT val lt EmptyBT) = min val (minElemOf lt)
minElemOf (NodeBT val EmptyBT rt) = min val (minElemOf rt)
minElemOf (NodeBT val lt rt) = if val  < (minElemOf lt) then min val (minElemOf rt)
else min (minElemOf lt) (minElemOf rt) 

maxElemOf :: Ord a => BinTree a -> a
maxElemOf (NodeBT val EmptyBT EmptyBT) = val
maxElemOf (NodeBT val lt EmptyBT) = max val (maxElemOf lt)
maxElemOf (NodeBT val EmptyBT rt) = max val (maxElemOf rt)
maxElemOf (NodeBT val lt rt) = if val  > lVal 
                                then max val rVal
                                else max lVal rVal
                                where lVal = maxElemOf lt
                                      rVal = maxElemOf rt 

foldBinTree :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBinTree _ startval EmptyBT = startval
foldBinTree f start (NodeBT val lt rt) = f val (foldBinTree f start lt) (foldBinTree f start rt)   

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT val lt rt)= NodeBT (f val) (mapBT f lt) (mapBT f rt)  


