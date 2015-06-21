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
