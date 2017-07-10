
fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

isDivisor :: Integral a => [a] -> Bool
isDivisor (x : y : _) | (y `mod` x) == 0 = True

isDivisor _ = False

isDivisor3 :: Integral a => [a] -> Bool
isDivisor3 (x : y : z : _) | (z `mod` x) == 0 = True
isDivisor3 _ = False