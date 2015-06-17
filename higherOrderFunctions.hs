compareWith100 :: (Num a, Ord a) => a -> Ordering
compareWith100 = compare 100

-- What is the difference between Fractional and floating
divideBy10 :: (Floating a) => a -> a
divideBy10 = (/10)

subtract4 :: (Num a) => a -> a
subtract4 = subtract 4

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

map' :: (a -> c) -> [a] -> [c]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- Why doesn't the following work :
-- filter' :: (Bool c) => (a -> c) -> [a] -> [a]
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x == True = x:filter' f xs
    |otherwise = filter' f xs

quickSort :: (Num a, Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = smallerSorted ++ [x] ++ largerSorted
    where smallerSorted = quickSort $ filter' (<= x) xs
          largerSorted = quickSort $ filter' (> x) xs

-- A function to find the the largest the number below x divisible by y.
-- Notes to self : mod takes Integral.
divfunc :: (Integral a) => a -> a -> a
divfunc x y = head ( filter f [x , (x-1) ..])
    where f a = if mod a y == 0 then True else otherwise == False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x:takeWhile' f xs
    |otherwise = []

-- A function to find all the even squares below x
evenSquares1 :: (Integral a) => a -> a
evenSquares1 x = sum ( takeWhile' (<x) (filter' even (map' (^2) [1 .. ])))
evenSquares2 :: (Integral a) => a -> a
evenSquares2 x = sum ( takeWhile' (<x) [a^2 | a <- [1 ..], even (a^2)])

{-- How can this compile (How can something be both Integral and Fractional)? And why is it not running if compiling?
chain :: (Integral a, Fractional a) => a -> [a]
chain x
    | x == 1 = [1]
    | even x = evenChain:chain evenChain
    |otherwise = oddChain:chain oddChain
    where evenChain = (x/2)
          oddChain = (3*x)+1
--}



chain :: (Integral a) => a -> [a]
chain x
    | x == 1 = [1]
    | even x = x:chain evenChain
    |otherwise = x:chain oddChain
    where evenChain = (div x 2)
          oddChain = (3*x)+1

-- Chains of length greater than y and of numbers below x
-- To compare, Integral can't be used. Why? {y here can't be Integral}
-- Why can't x be Int ? (To set the [..] range)
chainsGreaterThanV1 :: (Integral a1) => a1 -> Int -> Int
chainsGreaterThanV1 x y = sum ( map (\a -> if a > y then 1 else 0) (map f [1 .. x]))
    where f x = length(chain x)

chainsGreaterThanV2 :: (Integral a) => a -> Int -> Int
chainsGreaterThanV2 x y = length ( filter (==True) (map (f y) [1 .. x]))
    where f y x = length(chain x) > y

chainsGreaterThanV3 :: (Integral a) => a -> Int -> Int
chainsGreaterThanV3 x y = length (filter isLong (map chain [1..x]))
    where isLong xs = length xs > y
