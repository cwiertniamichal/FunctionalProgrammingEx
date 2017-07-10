
absInt :: Int -> Int
absInt n | n > 0  = n
        | n < 0 = -n

absInt' :: Int -> Int
absInt' n | n > 0 = n
        | otherwise = -n

sgn :: Int -> Int
sgn n | n > 0   = 1
    | n < 0     = -1
    | otherwise = 0 

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) | (a <= b) && (a <= c)   = a
                | (b <= a) && (b <= c)      = b
                | otherwise                 = c   

toUpperGuards :: Char -> Char
toUpperGuards a | (fromEnum a >= 97) && (fromEnum a <= 122) = toEnum (fromEnum a - 32)
                | otherwise                                 = a 

toLowerGuards :: Char -> Char
toLowerGuards a | (fromEnum a >= 65) && (fromEnum a <= 90) = toEnum (fromEnum a + 32)
                | otherwise                                = a

isDigitGuard :: Char -> Bool
isDigitGuard a | (fromEnum a >= 48) && (fromEnum a <= 57) = True
            | otherwise = False

charToNumGuards :: Char -> Int
charToNumGuards a | otherwise  = fromEnum a

romanDigitGuards :: Char -> String
romanDigitGuards a | fromEnum a == 48 = ""
                | fromEnum a == 49 = "I"
                | fromEnum a == 50 = "II"
                | fromEnum a == 51 = "III"
                | fromEnum a == 52 = "IV"
                | fromEnum a == 53 = "V"
                | fromEnum a == 54 = "VI"
                | fromEnum a == 55 = "VII"
                | fromEnum a == 56 = "VIII"
                | fromEnum a == 57 = "IX"
                | otherwise = "Wrong character"