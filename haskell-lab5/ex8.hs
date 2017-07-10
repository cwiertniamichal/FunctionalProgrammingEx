newtype Box a = MkBox a deriving (Show)

instance Applicative Box where
    pure = MkBox
    (MkBox f) <*> w = fmap f w

instance Functor Box where
    fmap f (MkBox x) = MkBox (f x)

newtype MyTriple a = MyTriple (a, a, a) deriving Show

instance Functor MyTriple where
    fmap f (MyTriple (x, y, z)) = MyTriple (f x, f y, f z) 

instance Applicative MyTriple where
    pure x = MyTriple (x, x, x)
    (MyTriple (_, _, f)) <*> w = fmap f w 

foo1 = do
    putStr "Computer "
    putStrLn "Science"

foo2 = do
    l <- getLine 
    putStrLn l
    putStrLn l
foo3 = do
    l <- getLine
    case (length l) > 10 of
        True -> putStrLn "Too long"
        otherwise -> putStrLn l