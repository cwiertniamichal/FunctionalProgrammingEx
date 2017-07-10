
newtype MyInt = MkMyInt Int

instance Eq MyInt where
    (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
    (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
    (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
    (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
    (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2) 
    negate (MkMyInt i)            = MkMyInt (negate i)
    abs (MkMyInt i)               = MkMyInt (if i < 0 then -1 else i)
    signum (MkMyInt i)            = MkMyInt (signum i)
    fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
    show (MkMyInt i) = "MkMyInt " ++ show i


data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance (Eq a) => Eq (BinTree a) where
    (==) EmptyBT EmptyBT = True
    (==) EmptyBT _ = False
    (==) _ EmptyBT = False
    (==) (NodeBT val1 lt1 rt1) (NodeBT val2 lt2 rt2) = (val1 == val2) && (lt1 == lt2) && (rt1 == rt2)
    

data Cart3DVec a = Cart3DVec {x::a,  y::a,  z::a} deriving (Show)

instance Eq a => Eq (Cart3DVec a) where
    (==) (Cart3DVec x1 y1 z1) (Cart3DVec x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2 

instance Ord a => Ord (Cart3DVec a) where
    (<=) (Cart3DVec x1 y1 z1) (Cart3DVec x2 y2 z2) = x1 <= x2 && y1 <= y2 && z1 <= z2

instance (Ord a, Num a) => Num (Cart3DVec a) where
    (+) (Cart3DVec x1 y1 z1) (Cart3DVec x2 y2 z2) = (Cart3DVec (x1+x2) (y1+y2) (z1+z2))
    (*) (Cart3DVec x1 y1 z1) (Cart3DVec x2 y2 z2) = (Cart3DVec (x1*x2) (y1*y2) (z1*z2))
    (-) (Cart3DVec x1 y1 z1) (Cart3DVec x2 y2 z2) = (Cart3DVec (x1-x2) (y1-y2) (z1-z2)) 
    abs (Cart3DVec x1 y1 z1) = (Cart3DVec (if x1 < 0 then -x1 else x1) (abs y1) (abs z1))
    signum (Cart3DVec x1 y1 z1) = Cart3DVec (signum x1) (signum y1) (signum z1)
    fromInteger x = Cart3DVec (fromIntegral x)  (fromIntegral x)  (fromIntegral x)

data Fraction a = Fraction {num :: a, denom :: a}
 
instance Show a => Show (Fraction a) where
    show (Fraction num denom) = "num" ++ show num ++ "denom" ++ show denom  

instance Num a => Num (Fraction a) where
    (+) (Fraction a1 b1) (Fraction a2 b2) = (Fraction (a1+a2) (b1+b2))
    (-) (Fraction a1 b1) (Fraction a2 b2) = (Fraction (a1-a2) (b1-b2))
    (*) (Fraction a1 b1) (Fraction a2 b2) = (Fraction (a1*a2) (b1*b2))
    abs (Fraction a1 b1) = Fraction (abs a1) (abs b1)
    signum (Fraction a1 b1) = Fraction (signum a1) (signum b1)
    fromInteger int = Fraction (fromIntegral int) (fromIntegral int)




data Example a b = Example {x1::a, x2::b}|
                   Example2 {y1::a, y2::b}

newtype Ex a = Ex {ala::a}

data Foo a = MkFoo { value :: a, name :: String }

instance (Show a) =>  Show (Foo a) where
    show MkFoo{ value = v, name = n } = "Name: " ++ n ++ " with " ++ show v

data AorB a b = A a | B b 

instance (Eq a, Eq b) => Eq (AorB a b) where
    (==) (A a1) (A a2) = a1 == a2
    (==) (B a1) (B a2) = a1 == a2
    (==) _ _ = False


