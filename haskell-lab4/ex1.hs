

polarToCartesian :: Floating a => (a, a) -> (a, a)
polarToCartesian (r, phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a, a)
type PolarCoord' a = (a, a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r, phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a, a) deriving (Show)
newtype PolarCoord'' a = MkPolarCoord'' (a, a) deriving (Show)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r, phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

sfericToCartesian :: Floating a => (a, a, a) -> (a, a, a)
sfericToCartesian (r, theta, phi) = (r * sin theta * cos phi, 
    r * sin theta * sin phi, r * cos theta)

cylindricToCartesian :: Floating a => (a, a, a) -> (a, a, a)
cylindricToCartesian (q, phi, z) = (q * cos phi, q * sin phi, z)

type SfericCoord a = (a, a, a)
type CylindricCoord a = SfericCoord a
type CartesianCoord3D a = SfericCoord a

sfericToCartesian' :: Floating a => SfericCoord a -> CartesianCoord3D a
sfericToCartesian' (r, theta, phi) = (r * sin theta * cos phi, 
    r * sin theta * sin phi, r * cos theta)

cylindricToCartesian' :: Floating a => CylindricCoord a -> CartesianCoord3D a
cylindricToCartesian' (q, phi, z) = (q * cos phi, q * sin phi, z)

newtype SfericCoordType a = MkSfericCoordType (a, a, a) deriving Show
newtype CylindricCoordType a = MkCylindricCoordType (a, a, a) deriving Show
newtype CartesianCoord3DType a = MkCartesianCoord3DType (a, a, a) deriving Show

sfericToCartesian'' :: Floating a => 
    SfericCoordType a -> CartesianCoord3DType a
sfericToCartesian'' (MkSfericCoordType (r, theta, phi)) = 
    MkCartesianCoord3DType (r * sin theta * cos phi, 
    r * sin theta * sin phi, r * cos theta)

cylindricToCartesian'' :: Floating a => 
    CylindricCoordType a -> CartesianCoord3DType a
cylindricToCartesian'' (MkCylindricCoordType (q, phi, z)) = 
    MkCartesianCoord3DType (q * cos phi, q * sin phi, z)

personInfoToString :: (String, String, String) -> String
personInfoToString (nm, snm, addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType'
personInfoToString' (name, surname, address) = 
    "name: " ++ name ++ ", surname: " ++ surname ++ ", addr: " ++ address

newtype Name'' = MkName (String)
newtype Surname'' = MkSurname (String)
newtype Address'' = MkAddress (String)
newtype PersonInfo'' = MkPersonInfo (Name'', Surname'', Address'')

personInfoToString'' :: PersonInfo'' -> String
personInfoToString'' (MkPersonInfo(MkName(name), 
                      MkSurname(surname), 
                      MkAddress(address))) = 
    "name: " ++ name ++ ", surname: " ++ surname ++ ", addr: " ++ address