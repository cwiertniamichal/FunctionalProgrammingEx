
isPalindrome :: [Char] -> Bool
isPalindrome s = reverse s == s

getElemAtIdx :: [a] -> Int -> a
getElemAtIdx xs i = last (take (i + 1) xs)

capitalize :: [Char] -> [Char]
capitalize s = if s == []
    then []
    else if (fromEnum(head s) < 97) || (fromEnum(head s) > 122)
        then s
        else
        toEnum (fromEnum (head s) - 32) : tail s
