lastDigit :: Integer -> Integer
lastDigit n = mod n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

toDigits :: Integer -> [Integer]
toDigits i
    | i <= 0 = []
    | otherwise = toDigits (dropLastDigit i) ++ [lastDigit i]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith ($) (cycle [id, (*2)]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = foldl (+) 0

validate :: Integer -> Bool
validate i = (==0) $ rem sum 10
    where sum = sumDigits . doubleEveryOther . toDigits $ i
