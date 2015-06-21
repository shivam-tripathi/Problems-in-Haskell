import Data.List
import qualified Data.Map as M

-- Only specific functions can be imported by mentioning in () after the module name.
-- To avoid some functions, place "import <..> hiding ( .. )". This is to avoid name of same name import functions.
-- Another way is to use qualified after the import term.

-- Data.List

numberUnique :: (Eq a) =>  [a] -> Int
numberUnique = length . nub

splitIntoAlphabets :: [Char] -> [String]
splitIntoAlphabets = words.intersperse ' '

-- Implementation of intersperse
intersperse' :: a -> [a] -> [a]
intersperse' i xs = init $ foldr (\a acc -> a:i:acc ) [] xs

-- Implementation of intercalate
intercalate' :: [a] -> [[a]] -> [a]
intercalate' xs ys = foldr1 (\x acc -> x++xs++acc) ys

-- Implementation of transpose
-- transpose' :: [[a]] -> [[a]]
-- transpose xs =

-- Implementation of concat
concat' :: [[a]] -> [a]
concat' xs = foldr1 (\x acc -> x++acc) xs

-- Implementation of concatMap
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f xs = foldr1 (\x acc -> x++acc) $ map f xs

-- Implementation of and
and' :: [Bool] -> Bool
and' xs = foldr (\x acc -> if not x then False else acc) True xs

and'' :: [Bool] -> Bool
and'' [] = True
and'' (x:xs) = if x == False then False else and'' xs

-- foldl', foldr', foldl1' and foldr1' are stricter version of their counterparts

-- Implementation of or
or' :: [Bool] -> Bool
or' xs = foldr (\x acc -> if x then True else acc) False xs

or'' :: [Bool] -> Bool
or'' [] = False
or'' (x:xs) = x || (or'' xs)

-- Implementation of any
any' :: (a -> Bool) -> [a] -> Bool
any' f xs = or' . map f $ xs

-- Implementation of all
all' :: (a -> Bool) -> [a] -> Bool
all' f xs = and' . map f $ xs

check1 = all' (flip(elem) ['A' .. 'Z']) "HELLOhihelloHI"

-- Implementation of iterate
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x: (helper f x)
    where helper f x = iterate' f (f x)

-- splitAt' :: Int => a -> [a] -> ([a], [a])
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (before, after)
    where before = map (\x -> xs!!x) [0 .. (n-1)]
          after = map (\x -> xs!!x) [n .. (length xs - 1)]

-- Implementation of takeWhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x:(takeWhile' f xs)
    | otherwise = []

-- Implementation of dropWhile
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise = (x:xs)

-- Implementation of span
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' _ [] = ([], [])
span' f xs = ( (takeWhile' f xs), (dropWhile' f xs))

-- Implementation of break
-- break f xs is same as span (not.f $) xs
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' f xs = ((takeWhile (not.f $) xs), (dropWhile (not.f $) xs) )

-- Implementation of sort, based on quick sort method
sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' (x:xs) = lesser ++ [x] ++ greater
    where lesser = sort' (foldr (\a acc -> if a<x then a:acc else acc) [] xs)
          greater = sort' (foldr (\a acc -> if a>=x then a:acc else acc) [] xs)

-- Implementation of group
group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:xs) = let (fwd, aft) = (span' (==x) (x:xs)) in  (fwd : (group' aft))

-- New syntax, l will refer to (x:xs) if we place @.
check2  = map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]


-- Implementation of inits
inits :: [a] -> [[a]]




nub' :: (Eq a) => [a] -> [a]
nub' xs = foldl (\acc x -> if (elem x acc) then acc else acc++[x]) [] xs

delete' :: Char -> String -> String
delete' x (y:ys)
    | x == y = ys
    | otherwise = y:delete' x ys


{--
op :: (Eq a) => [a] -> [a] -> [a]
op [] _ = []
op ys [] = ys
op (y:ys) xs
    | elem y xs = op ys xs
    | otherwise = y: (op ys xs)
--}
