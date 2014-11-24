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

type Peg = String
type Move = (Peg, Peg)
type NumDisks = Integer
hanoi :: NumDisks -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n < 1 = []
    | n == 0 = [(a, b)]
    | otherwise = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)


