

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

doSafeTail3x :: [a] -> Maybe [a]
doSafeTail3x xs = do
    t1 <- safeTail xs
    t2 <- safeTail t1
    t3 <- safeTail t2
    return t3

safeTail3x :: [a] -> Maybe [a]
safeTail3x xs =
    safeTail xs >>= \t1 ->
        safeTail t1 >>= \t2 ->
            safeTail t2 >>= \t3 ->
                return t3

safeTail3x' :: [a] -> Maybe [a]
safeTail3x' xs = return xs >>= safeTail >>= safeTail >>= safeTail


f5 :: Int -> Int -> Int -> Int
f5 x y z = 1000 `div` x + 100 `div` y + 10 `div` z

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y | y /= 0    = Just $ x `div` y
            | otherwise = Nothing

safeF5 :: Int -> Int -> Int -> Maybe Int
safeF5 x y z =
    case (safeDiv 1000 x) of
        Nothing -> Nothing
        Just (iOverX) ->
            case (safeDiv 100 y) of
                Nothing -> Nothing
                Just (iOverY) ->
                    case (safeDiv 10 z) of
                        Nothing -> Nothing
                        Just (iOverZ) -> Just $ iOverX + iOverY + iOverZ


safeF5' :: Int -> Int -> Int -> Maybe Int
safeF5' x y z = do
    iOverX <- safeDiv 1000 x
    iOverY <- safeDiv 100 y
    iOverZ <- safeDiv 10 z
    return $ iOverX + iOverY + iOverZ

(>==>) :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
(>==>) f g a = f a >>= g  

f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x + 1) else Nothing

f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x /= 0 then Just (10 * x) else Nothing


joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe Nothing = Nothing
joinMaybe (Just x) = x

safeSum10DivX1 :: [Int] -> Maybe Int
safeSum10DivX1 [] = Just 0
safeSum10DivX1 (x:xs) = case x of
    0 -> Nothing
    _ -> case safeSum10DivX1 xs of
            Nothing -> Nothing
            (Just res) -> Just (res + 10 `div` x) 

safeNum x = case x of
    0 -> Nothing 
    _ -> Just x

safeSum10DivX1Do :: [Int] -> Maybe Int
safeSum10DivX1Do [] = Just 0
safeSum10DivX1Do (x:xs) = do
    num <- safeNum x
    res <- safeSum10DivX1Do xs
    return (res + 10 `div` num) 

safeSum10DivX1Bind [] = Just 0
safeSum10DivX1Bind (x:xs) = safeSum10DivX1Bind xs >>= \y ->  safeNum x >>= \z -> Just ( 10 `div` z + y)   


safeTail3x'' :: [a] -> Maybe [a]
safeTail3x'' xs = safeTail xs >>= safeTail >>= safeTail
