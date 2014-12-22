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
        show = show . take 42 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed $ streamFromSeed f (f seed)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a s1) s2 = Cons a (interleaveStreams s2 s1)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamFromSeed ((+1) . (`rem` 4)) 1)
