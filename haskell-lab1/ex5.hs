sgn :: Int -> Int
sgn a = if a > 0 
    then 1
    else if a == 0
        then 0
        else -1

absInt :: Int -> Int
absInt n = if n >= 0 
    then n
    else -n

min2Int :: (Int, Int) -> Int
min2Int (a, b) = if a >= b
    then b
    else a

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) = if (a <= b) && (a <= c)
    then a
    else if (b <= c) && (b <= a)
        then b
        else c

min3Int' :: (Int, Int, Int) -> Int
min3Int' (a, b, c) = if a <= b
    then min2Int (a, c)
    else min2Int (b, c)

toUpper :: Char -> Char
toUpper a = if (fromEnum a >= 97) && (fromEnum a <= 122)
    then toEnum (fromEnum a - 32) 
    else a

toLower :: Char -> Char
toLower a = if (fromEnum a >= 65) && (fromEnum a <= 90)
    then toEnum (fromEnum a + 32)
    else a

isDigit :: Char -> Bool
isDigit a = if (fromEnum a >= 48) && (fromEnum a <= 57)
    then True
    else False

charToNum :: Char -> Int
charToNum a = fromEnum a

romanDigit :: Char -> String
romanDigit a = if fromEnum a == 57 
then "IX"
else if fromEnum a == 56
then "VIII"
else if fromEnum a == 55
    then "VII"
    else if fromEnum a == 54
        then "VI"
        else if fromEnum a == 53
            then "V"
            else if fromEnum a == 52
                then "IV"
                else if fromEnum a == 51
                    then "III"
                    else if fromEnum a == 50
                        then "II"
                        else if fromEnum a == 49
                            then "I"
                            else 
                                ""