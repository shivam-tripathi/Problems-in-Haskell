-- Problem 1
mylast [] = error "Not result"
mylast (x:[]) = x
mylast (x:xs) = mylast xs

-- Problem 2
mysecondlast [] = error "No result"
mysecondlast (x:y:[]) = x
mysecondlast (x:y:xs) = mysecondlast (y:xs)

-- Problem 3 
kthelement :: [a] -> Int -> a
kthelement [] _ = error "Error"
kthelement (x:[]) _ = x
kthelement (x:xs) k = if ( k == 1) then x else (kthelement xs (k-1)) 

-- Problem 4 
listlength :: [a] -> Int
listlength (x:xs) = helper 1 xs
					where helper k [] = k
					      helper k (x:xs) = helper (k+1) xs

-- Problem 5
listReverse :: [a] -> [a]
listReverse [] = []
listReverse (x:[])= [x]
listReverse (x:xs) = (listReverse xs) ++ [x] 

-- Problem 6
listpalindrome :: Eq a => [a] -> Bool
listpalindrome list = list == (listReverse list) 