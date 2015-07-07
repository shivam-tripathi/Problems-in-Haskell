import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as M

-- Only specific functions can be imported by mentioning in () after the module name.
-- To avoid some functions, place "import <..> hiding ( .. )". This is to avoid name of same name import functions.
-- Another way is to use qualified after the import term.
-- @shivam-tripathi
-- Data.List

numberUnique :: (Eq a) =>  [a] -> Int
numberUnique = length . nub

splitIntoAlphabets :: [Char] -> [String]
splitIntoAlphabets = words.intersperse' ' '

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
inits' :: [a] -> [[a]]
inits' xs = [[]] ++ map (helper xs) [0 .. (length xs - 1)]
    where helper xs x = foldr (\a acc -> (xs!!a):acc) [] [0 .. x]

inits'' :: [a] -> [[a]]
inits'' [] = []
inits'' xs = (inits $ init xs) ++ [xs]

tails' :: [a] -> [[a]]
tails' [] = []
tails' xs = (map (\x -> (foldr (\a acc -> (xs!!a):acc) [] [x .. (l-1)])) [0 .. (l-1)]) ++ [[]]
    where l = length xs

tails'' ::  [a] -> [[a]]
tails'' [] = [[]]
tails'' xs = xs:(tails'' $ tail xs)


-- search a subset in set
-- A poor implementation.
subsets :: [a] -> [[[a]]]
subsets [] = []
subsets (x:xs) = (initis (x:xs)) : subsets xs
    where initis ys = map (\x -> (foldr (\a acc -> (ys !! a):acc ) [] [0 .. x])) [0 .. (length xs)]

{--
subsets' xs = length.removeSpaces.concat $  helper
    where helper = map (\x -> (foldl (\acc a -> (take x a):acc )) [] (tails xs)) [0 .. (length xs) ]
          removeSpaces xs = foldr (\a acc -> if a == ' ' then acc else a:acc ) [] xs
--}

isinfix ys xs = elem ys (concat $ subsets xs)

-- A nice implementation of search.
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- Implementation of isPrefixOf and isSuffixOf
isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' ys xs = elem ys (inits xs)

isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' ys xs = elem ys (tails xs)

-- Implementation of partition
partition' :: (a -> Bool) -> [a] -> ([a],[a])
-- partition' f [] = ([],[])
partition' f xs = ((filter f xs), (filter (not.f $) xs))

-- Implementation of find
find' :: (a -> Bool) -> [a] -> Maybe a
find' _ [] = Nothing
find' f (x:xs)
    | f x = Just x
    | otherwise = find' f xs

-- Implementation of elemIndex
elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' _ [] = Nothing
elemIndex' x xs = foldr (\a acc -> if (xs!!a) == x then (Just a) else acc ) Nothing [0 .. (length xs - 1)]

-- Implementation of elemIndices
elemIndices' :: (Eq a) => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x xs = foldr (\a acc -> if (xs!!a) == x then (a:acc) else acc ) [] [0 .. (length xs - 1)]

-- Implementation of findIndex
findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' _ [] = Nothing
findIndex' f xs = foldr (\a acc -> if f (xs!!a) then (Just a) else acc ) Nothing [0 .. (length xs - 1)]

-- Implementation of findIndices
findIndices' :: (a -> Bool) -> [a] -> [Int]
findIndices' _ [] = []
findIndices' f xs = foldr (\a acc -> if f (xs!!a) then (a:acc) else acc ) [] [0 .. (length xs - 1)]

-- zip exist till 7 lists, here is an implementation for 3 lists.
zip3' :: [a] -> [a] -> [a] -> [(a,a,a)]
zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' [] _ _ = []
zip3' (x:xs) (y:ys) (z:zs) =  (x,y,z): (zip3' xs ys zs)

-- zipWith also exist till 7. Similar implementation.

-- Implementation of lines

-- Implementation of words
words' :: String -> [String]
words' [] = []
words' xs = clear $ if (index /= Nothing) then (take (f index) xs):(words' $ helper) else [xs]
    where index = findIndex (== ' ') $ xs
          l = length xs - 1
          helper = foldr (\a acc -> (xs!!a):acc) [] [(f index + 1) .. l]
          f (Just a) = a
          clear ys = foldr (\a acc -> if a == "" then acc else a:acc) [] ys

words'' xs = filter (not.any isSpace).(groupBy' (on (==) isSpace)) $ xs

words''' xs = filter (not.(==) " ") (groupBy' (on (==) isSpace) xs)

words'''' xs = foldr (\a acc -> if a == " " then acc else a:acc) [] (groupBy' (on (==) isSpace) xs)

--Implementation of unlines
unlines' xs = foldr (\x acc -> x++"\n"++acc) [] xs

-- Implementation of unwords
unwords' xs = foldr (\x acc -> x++" "++acc) [] xs

-- Implementation of nub
nub' :: (Eq a) => [a] -> [a]
nub' xs = foldl (\acc x -> if (elem x acc) then acc else acc++[x]) [] xs

nub'' :: (Eq a) => [a] -> [a]
nub'' [] = []
nub'' (x:xs) = if elem x xs then nub'' xs else x:(nub'' xs)

-- Implementation of delete
delete' :: (Eq a) => a -> [a] -> [a]
delete' x (y:ys)
    | x == y = ys
    | otherwise = y:delete' x ys

-- What does (**) operator do?

-- Implementation of (\\)
op xs [] = xs
op xs (y:ys) =  (op) (delete' y xs) ys
-- How to declare infix functions ?

-- Implementation of union
union' :: (Eq a) => [a] -> [a] -> [a]
union' xs ys = foldl (\acc a -> if elem a xs then acc else acc++[a] ) xs ys

-- Implementation of intersect
intersect' :: (Eq a) => [a] -> [a] -> [a]
intersect' xs ys = foldr (\a acc -> if elem a xs then a:acc else acc) [] ys

-- Implementation of insert
insert' :: (Ord a) => a -> [a] -> [a]
insert' n [] = [n]
insert' n (x:xs)
    | n <= x = n:x:xs
    | otherwise = x : (insert' n xs)

-- length, take, drop, splitAt, !! and replicate :: Int as one of the parameters
-- To solve the problem of Fractional Integer error, generic functions have been provided in Data.List.
-- For example to divide a number by length of a list, one can't use length.
-- Use genericLength, genericTake, genericDrop, genericSplitAt, genericIndex and genericReplicate.

-- The nub, delete, union, intersect and group functions all have their more general counterparts called
--  nubBy, deleteBy, unionBy, intersectBy and groupBy i.e. nub, delete, union, intersect and group have
--  only conditions of (==), while others can be given a boolean returning function.

-- Implementation of nubBy
-- Implementation of deleteBy
-- Implementation of intersectBy
-- Implementation of unionBy
-- Implementation of groupBy


groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' f (x:xs) = let (insert, pass) = (span (f x) xs) in ([(x:insert)]++(groupBy f pass))

-- Use of 'Data.Function.on' function

-------------------------------------------------------------------------------

-- Data.Char

-- isControl'

isSpace' ch = ch == ' '

isLower' ch = elem ch ['a' .. 'z']

isUpper' ch = elem ch ['A' .. 'Z']

isAlpha' ch = or [ isLower' ch, isUpper' ch]

isDigit' ch = elem ch ['0' ..'9']

isAlphaNum' ch = or [ isAlpha' ch, isDigit' ch]

isPrint' ch = not.isControl $ ch

-- isOctDigit'

-- isHexDigit'

-- What is the difference between isLetter and isAlpha

-- isMark'

-- What is the difference between isDigit and isNumber

-- isPunctuation'
-- # is is Punctuation character?

-- isSymbol'

-- isAscii', isAsciiUpper', isAsciiLower

-- isLatin1

check3 xs = groupBy' ((==) `on` isSpace) xs

-- Implementation of generalCategory using simple comparison, not the enumeration
generalCategory' ch
    | isControl ch = "Control character"
    | isSpace ch = "Space"
    | isLower ch = "Lower case alphabet"
    | isUpper ch = "Upper case alphabet"
    | isAlpha ch = "Alphabet"
    | isDigit ch = "Digit"
    | isAlphaNum ch = "Alphabet or Digit"
    | isOctDigit ch = "Octadecimal"
    | isHexDigit ch = "Hexadecimal"
    | isPunctuation ch = "Punctuation mark"
    | isSymbol ch = "Mathematical symbol"
    | isAscii ch = "Ascii symbol - first 128 characters of Unicode"
    | isAsciiUpper ch = "Ascii upper case alphabet"
    | isAsciiLower ch = "Ascii Lower case alphabet"
    | isLatin1 ch = "Falls in first 256 characters of Unicode"
    | isMark ch = "Unicode mark"
    | isPrint ch = "Printable character"


-- ord, chr, toEnum, fromEnum

-- Implementation of toUpper, toLower
toUpper' ch
    | isLower' ch = chr $ ord ch - 32
    | otherwise = error "Not a lower case letter"

toLower' ch
    | isUpper' ch = chr $ ord ch + 32
    | otherwise = error "Not a upper case letter"

-- Difference between uppercase and titlecase?

-- Implementation of digitToInt
digitToInt' :: Char -> Int
digitToInt' ch
    | isDigit ch = ord ch - 48
    | ch >= 'a' && ch <= 'f' = ord ch - ord 'a' + 10
    | ch >= 'A' && ch <= 'F' = ord ch - ord 'A' + 10
    | otherwise = error "not a digit"

intToDigit' :: Int -> Char
intToDigit' i
    | i >= 0 && i <= 9 = chr ( i + 48)
    | i > 9 && i <= 15 = chr ( ord 'a' + i - 10)
    | otherwise = error "not a digit"

caesarCipherEncode :: Int -> [Char] -> [Char]
caesarCipherEncode shift mssg = map (chr .(+) shift . ord) mssg

caesarCipherDecode :: Int -> [Char] -> [Char]
caesarCipherDecode shift mssg = map (chr.(+) (negate shift). ord) mssg

--------------------------------------------------------------------------------
-- Data.Map
-- Association list (also called dictionaries) are lists, using key-value pairs.

phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

findKey :: String -> [(String, String)] -> Maybe String
findKey _ [] = Nothing
findKey key ((a,b):xs)
    | key == a = Just b
    | otherwise = findKey key xs

findKey' :: String -> [(String, String)] -> Maybe String
findKey' key = foldl (\acc (k,v) -> if k == key then Just v else acc) Nothing

-- Data.Maps offer faster Association lists which have been internally
-- implemented as trees.

-- fromList takes association list in form of a list and returns a map.
-- M.fromList discards duplicate keys. The last one will be considered.

newMap = M.fromList [("betty","555-2938"), ("bonnie","452-2928"),
    ("patsy","493-2928"), ("lucille","205-2928"), ("wendy","939-8282"),
    ("penny","853-2492")]

-- :t M.fromList
-- M.fromList :: Ord k => [(k, a)] -> M.Map k a
-- Maps must have keys belonging to Ord class.
-- M.empty gives empty map => fromList []
test = M.empty

-- M.insert inserts a key-value pair in the map.
insertElem k v = M.insert k v test

fromList xs = foldl (\acc (k,v) -> M.insert k v acc) M.empty xs

-- M.null checks if the map is empty.
ans = M.null test

-- M.size returns the size.
newMapLen = M.size newMap

-- M.singleton takes a key-value and returns map initialised with this pair.

-- M.lookup takes a key and a map, checks if the key exists, and return Just
-- value.

-- toList is reverse of fromList.
