fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)
-- Recursive function defined
myfiblist = map fib [1..]
-- Infinite list defined
fiblist = 1:1: zipWith (+) fiblist (tail fiblist)
-- Copied from class example 

data Stream a = Cons a (Stream a)
instance Show a => Show (Stream a) where 
	show ( Cons a b ) = show ( take 20 (streamList (Cons a b)))

streamList :: Stream a -> [a]
streamList (Cons a b) = a : (streamList b)
-- Definition of Stream and instance of Show for Stream 

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)
-- Implementation of streamRepeat

streamMap :: (a->b) -> Stream a -> Stream b
streamMap (f) (Cons x y) = Cons (f x) (streamMap f y) 
-- Implementation of streamMap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed (k) a = Cons a (streamFromSeed (k) (k a))
-- Implementation of streamFromSeed

nats :: Stream Integer
nats = streamFromSeed (succ) 1

firstStream = Cons streamFromSeed 