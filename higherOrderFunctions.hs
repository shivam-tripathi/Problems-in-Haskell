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

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

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

-- For now, Integral used to set limit to ranges, can be applied to many arguments.
-- Int is applied to one argument at a time.
-- mod takes Integral.
-- What are the differences between Num and Integral?
-- length returns Int instead of Num. Num vs Int?
-- What is Enum?
-- fromIntegral converts Int to Num. (What's the use? Isn't Int just fine?)

-- List of Curried functions

-- Didn't understand the function's declaration????
-- listOfCurriedFunc :: (Num a) => (a -> a -> a) -> Int -> [(a -> a -> a)]
listOfCurriedFunc :: (Enum a, Num a) => (a -> b) -> a -> [b]
listOfCurriedFunc f x = map f [0 .. x]

-- Lambda functions
-- Pattern matching in Lambda functions
multiply :: (Num a) => [(a,a)] -> [a]
multiply xs = map (\(a,b) -> a*b) xs

addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

-- \x means that a argument has been passed to it.
multThree :: (Num a) => a -> a -> a -> a
multThree = \x -> \y -> \z -> x*y*z

-- In case of curried functions, they can take any argument.
-- We cannot determine the type.
-- Notations like t1, t2, t are taken to specify types.
flip'' :: (t1 -> t2 -> t) -> t2 -> t1 -> t
flip'' f = \ x y -> f y x

-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- Implementation of foldl
foldl' :: (t1 -> t -> t1) -> t1 -> [t] -> t1
foldl' f init [x] = f init x
foldl' f init (x:xs) = (foldl' f) init' xs where init' = f init x

-- Implementation of sum function using foldl
sum' :: (Num a, Ord a) => [a] -> a
sum' (x:xs) = foldl (+) x xs

sum'' :: (Num a, Ord a) => [a] -> a
sum'' = foldl (+) 0

-- Implementation of the elem func using foldl
elem' :: (Num a, Ord a) => a -> [a] -> Bool
elem' a xs = foldl (\acc b -> if a==b then True else acc) False xs

-- Implementation of map by foldl
mapl' :: (a -> b) -> [a] -> [b]
mapl' f xs = reverse (foldl (\acc a -> (f a):acc) [] xs)

-- Implementation of map using foldr
mapr' :: (a -> b) -> [a] -> [b]
mapr' f xs = foldr (\x acc -> (f x):acc ) [] xs

-- What is the implentation of foldr?
