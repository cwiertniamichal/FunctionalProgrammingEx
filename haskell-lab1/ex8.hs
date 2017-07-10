notcase :: Bool -> Bool
notcase b = case b of
            True -> False
            False -> True 

absIntCase n = 
    case (n >= 0) of
        True -> n
        _ -> -n

isItTheAnswerCase :: String -> Bool
isItTheAnswerCase s = 
    case s of
        "Love" -> True
        _ -> False

orCase :: (Bool, Bool) -> Bool
orCase (a, b) = 
    case (a, b) of
        (False, False) -> False
        _ -> True  

andCase :: (Bool, Bool) -> Bool
andCase (a, b) = 
    case (a, b) of
        (True, True) -> True
        _ -> False

nandCase :: (Bool, Bool) -> Bool
nandCase (a, b) = 
    case (a, b) of 
        (True, True) -> False
        _ -> True

xorCase :: (Bool, Bool) -> Bool
xorCase (a, b) = 
    case (a, b) of
        (True, True) -> False
        (False, False) -> False
        _ -> True
