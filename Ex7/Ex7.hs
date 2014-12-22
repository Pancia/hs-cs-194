module Ex7 where

import System.Random

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

sCycle :: [a] -> Stream a
sCycle [] = error "empty list"
sCycle cy = sCycle' cy cy
      where sCycle' cy []     = sCycle' cy cy
            sCycle' cy (c:cs) = Cons c (sCycle' cy cs)

interleaveS :: Stream a -> Stream a -> Stream a
interleaveS (Cons a s1) s2 = Cons a (interleaveS s2 s1)

ruler :: Stream Integer
ruler = interleaveS (sRepeat 0) (sCycle [1,2,1,3,1,2,1,4])

randomList :: (Random a, RandomGen g) => g -> [a]
randomList g = (\(x, g') -> x : randomList g') (random g)

randomInts :: Int -> [Int]
randomInts n = take n $ randomList (mkStdGen 5)

--907 MB
minMax :: [Int] -> Maybe (Int, Int)
minmax [] = Nothing
minMax xs = Just (minimum xs, maximum xs)

--994 MB
minMax' :: [Int] -> Maybe (Int, Int)
minMax' [] = Nothing
minMax' xs@(x:_) = Just $ foldl (\(mn, mx) x -> ((min mn x), (max mx x))) (x, x) xs

main :: IO ()
main = print $ minMax' $ randomInts 1000000
