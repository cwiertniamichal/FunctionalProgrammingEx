
 fiveToPower_ :: Integer -> Integer
 fiveToPower_ = (5 ^)

 _ToPower5 :: Num a => a -> a
 _ToPower5 = (^ 5)

 subtrNFrom5 :: Num a => a -> a
 subtrNFrom5 = (5 -)

 subtr5From_ :: Num a => a -> a
 subtr5From_ = flip (-) 5

 flip2 :: (a -> b -> c) -> b -> a -> c
 flip2 f y x = f x y

 flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
 flip3 f z y x = f x y z

 test_foo x y z = x - y - z