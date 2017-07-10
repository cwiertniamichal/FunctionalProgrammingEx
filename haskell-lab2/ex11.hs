sum'3 :: Num a => [a] -> [a] -> a
sum'3 = loop 0
    where loop acc [] []    = acc
          loop acc [] (y:ys) = loop (y + acc) [] ys
          loop acc (x:xs) []    = loop (x + acc) xs [] 
          loop acc (x:xs) (y:ys) = loop (y + x + acc) xs ys

selectEven :: [Integer] -> [Integer]
selectEven = loop []
    where loop acc [] = acc
          loop acc (x:xs) = if even x 
                then loop (acc ++ [x]) xs
                else loop acc xs