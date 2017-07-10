concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concatUsingListCoprehension ys = [x | xs <- ys, x <- xs]

concatUsingFoldr :: [[a]] -> [a]
concatUsingFoldr = foldr (++) []
