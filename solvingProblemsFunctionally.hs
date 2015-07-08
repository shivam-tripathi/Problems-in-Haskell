import Data.Char
import Data.List

-- reversePolish xs = (listNumber xs)

reversePolishNotation :: String -> Int
reversePolishNotation xs = head $ foldl folder [] (words xs)
    where   folder = (\acc x -> if isNum x then (toNumber x):acc else (copycat acc x))
            copycat (a:b:xs) op = (operate op b a):xs
            toNumber zs = foldl1 (\acc x -> acc * 10 + x) (map (subtract 48. ord $) zs)
            isNum zs = and $ map isDigit zs
            operate op x y
                | op == "+" = x+y
                | op == "-" = x-y
                | op == "*" = x*y
                | op == "/" = div x y
                | op == "^" = x ^ y
                | otherwise = error "wrong symbol"

-- Once we specify the return type of the function solveRPN, by type inference
-- haskell deduces that the list is also Float, as the head ( which is being
-- returnes here) is Float. This ensures that read function reads numberString
-- as a Float only, although it could have been specified explicitly too. Also
-- making it float helps to implement function like log, (/), etc which require
-- Float type input.
-- Concept of stacks can be implemented easily, as the head of any list is easy
-- to replace, and in case of using cons, the head is the last item inserted.
-- Using pattern matching, we can easily implement RPN expressions. If it is a
-- number, insert it. If it is not a number, take required number of items from
-- the begining of the stack, operate the operator on them, and insert the
-- result at the head.

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:y:ys) "^" = (y ** x):ys
            foldingFunction (x:xs) "ln" = log x:xs
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = read numberString:xs

