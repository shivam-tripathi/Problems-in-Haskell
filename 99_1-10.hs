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

-- Problem 7
-- data NestedList a = Elem a| List [NestedList a]
-- flatten :: NestedList a -> [a] 
-- flatten Elem a  = [a]
-- flatten (List ((Elem x):xs)) = [x] ++ (flatten xs) 
-- [1:2:[3:4:[]]:5:6:[7:8:9:[]]]
-- flatten ((Elem x):xs) = x : (map flatten xs)  

-- Problem 8 
{-- This works too, but it's brute force :P
compress :: Eq a =>[a] -> [a]
compress [] = []
compress (x:xs) = x : (callagain (x:xs) 1)

callagain :: Eq a =>[a] -> Int -> [a]
callagain [] _ = []
callagain xs index =  if (index < length xs ) then if ((xs!!index) /= ( xs!! (index-1))) then (xs!!index) : ( callagain xs (index + 1)) else ( callagain xs (index + 1)) else []

--}
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (callagain xs x)
callagain [] _ = []
callagain (x:xs) y = if ( x == y ) then (callagain xs) y else (x : callagain xs x)

-- Problem 9
