data CartInt2DVec = MkCartInt2DVec X Y 

type X = Int
type Y = Int


xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a deriving (Show)

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVecRecord2 a = MkCart2DVecRecord2 {x::a, y::a} deriving (Show)

data List a = EmptyL | Cons a (List a) deriving Show

headMy :: List a -> a
headMy EmptyL = error "headMy: the empty list has no head!"
headMy (Cons x xs) = x

data ThreeColors = Blue |
                   White |
                   Red


type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red = "Irene Jacob"


data Cart3DVec a = Cart3DVec a a a

xCoord3D :: Cart3DVec a -> a
xCoord3D (Cart3DVec x _ _) = x

yCoord3D :: Cart3DVec a -> a
yCoord3D (Cart3DVec _ y _) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (Cart3DVec _ _ z) = z

data Cart3DVecRecord a = Cart3DVecRecord {xr :: a, yr :: a, zr :: a}
data PolarVec2D a = PolarVec2D a a

r :: PolarVec2D a -> a
r (PolarVec2D r _) = r

theta :: PolarVec2D a -> a
theta (PolarVec2D _ theta) = theta

data PolarVec2DRecord a = PolarVec2DRecord {rr :: a, thetar :: a}

polarToCartesian :: Floating a => PolarVec2D a -> Cart2DVec' a
polarToCartesian (PolarVec2D r theta) = MkCart2DVec' (r * cos theta) (r * sin theta)

polarToCartesianRecord :: Floating a => PolarVec2DRecord a -> Cart2DVecRecord2 a
polarToCartesianRecord (PolarVec2DRecord {rr = r, thetar = theta}) = 
    MkCart2DVecRecord2 {x = r * cos theta, y = r * sin theta}

data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b

data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving (Show)

rootValue :: Tree a -> a
rootValue EmptyT = error "Empty tree has no root"
rootValue (Node val _ _) = val

data TrafficLights = RedLight |
                     YellowLight |
                     GreenLight
data Action = Wait |
              Set |
              Go deriving (Show)

actionFor :: TrafficLights -> Action
actionFor RedLight = Wait
actionFor YellowLight = Set
actionFor GreenLight = Go