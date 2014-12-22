module Ex7 where

fib :: Integer -> Integer
fib n
  | n <= 0 = 0
  | n <= 2 = 1
  | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

fibs2 :: [Integer]
fibs2 = [1,1] ++ zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)
instance Show a => Show (Stream a) where
        show = show . take 42 . sToList

sToList :: Stream a -> [a]
sToList (Cons a s) = a : sToList s

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sMap :: (a -> b) -> Stream a -> Stream b
sMap f (Cons a s) = Cons (f a) (sMap f s)

sFromSeed :: (a -> a) -> a -> Stream a
sFromSeed f seed = Cons seed $ sFromSeed f (f seed)

nats :: Stream Integer
nats = sFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a s1) s2 = Cons a (interleaveStreams s2 s1)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamFromSeed ((+1) . (`rem` 4)) 1)
