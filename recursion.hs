factorial :: Int -> Int
factorial 0 = 0
factorial 1 = 1
factorial x = (factorial (x-1)) + (factorial(x-2))

max' :: (Ord a) => a -> a -> a
max' a b
    | (a > b) = a
    |otherwise = b

-- Implement max function
maximum'  :: (Ord a) => [a] -> a
maximum' [] = error "Empty list has no maximum."
maximum' (x:[]) = x
maximum' (x:xs) = max' x maxtail
    where maxtail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Empty list has no maximum"
maximum'' (x:[]) = x
maximum'' (x:xs) =
    let maxtail = maximum'' xs
    in max' x maxtail

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' n x
    | n<=0 = []
    |otherwise = x:replicate' (n-1) x

-- Why is Num not a subclass of Ord? Aren't numbers always ordered? Is it because Int and Fractional are subclasses of Num?

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) =  x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y): zip xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs)
    |n == x = True
    |otherwise = elem' n xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = smallerSorted ++ [x] ++ largerSorted
    where smallerSorted = quickSort [a | a <- xs, a <= x ]
          largerSorted = quickSort [a | a <- xs, a > x]

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerSorted = quicksort' [a | a <- xs, a <= x]
        biggerSorted = quicksort' [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

sum' :: (Num a, Ord a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: (Num a, Ord a) => [a] -> a
product' xs
    | xs == [] = error "No numbers to multiply"
product' [x] = x
product' (x:xs) = x * product' xs
