import Data.Char
import Control.Monad
-- :t putStrLn
-- putStrLn :: String -> IO()

-- IO() is IO action, carries out side-effect action, returning dummy value ().
-- :t ()
-- () :: ()
-- Type of empty tuple is empty tuple, and has value also of empty tuple.

main = do
    main_'

-- 'do' is used to bind together a number a IO actions.

main' = do
    a <- putStrLn "Hello! What's your name my friend?"
    print a
    name <- getLine
    putStrLn ("Hey "++ name++", nice to meet you.")

-- :t getLine
-- getLine :: IO String
-- By convention, we don't specify the type declaration of IO function.
-- (<-) is used before getLine, it means that perform getLine function and bind
-- it's result to the variable on the other side.
-- This type of binding can occur only in an IO function.
-- getLine is impure in the sense that it will not return same value when called
-- again.
-- getLine returns IO action returning a String, and this is temporarily
-- converted into pure String form using (<-) binding.
-- In short, to deal with impure data, we need an impure environment.

-- Every IO action has a result encapsulated with it, putStrLn, print gives ()
-- upon binding, while getLine returns String upon binding.

-- The last IO statement cannot be bound to any name, it's result is bound to
-- the function's return.

-- name = getLine will only make name similar in action to getLine, will not
-- the result of getLine to name.

main'' = do
    lineString <- getLine
    if null lineString then
        return ()
    else do
        let line = unwords.map reverse.words $ lineString
        putStrLn line
        main''

-- if <..> then IO Action else IO Action
-- return statement makes an IO action out of pure value (I think it is sort of
-- of opposite to <- notation). It makes a bogus IO action.
-- This IO action doesn't do anything, it just has value encapsulated in it.
-- As a result, an IO function can have multiple return statements, and as long
-- as they are not bound any name, they are of no consequence.

-- else here is followed by two IO statements therefore they are glued together
-- by a do statement.

main_ = do
    a <- return "hello"
    b <- return "hi"
    putStrLn $ a++b

-- putStr prints without skipping to next line

-- Implementation of putStr and putStrLn
putStr' [] = return ()
putStr' (x:xs) = do    putChar x
                       putStr' xs

putStrLn' [] = putChar '\n'
putStrLn' (x:xs) = do  putChar x
                       putStrLn' xs

print' x = putStrLn $ show x

main_' = do
    ch <- getChar
    if ch /= ' '
        then do
            putChar ch
            main_'
    else return ()

-- To remove redundant else, when can be used. It is imported from Control.Monad

main_'' = do
    ch <- getChar
    when (not.(==) ch $ ' ') $ do
        putChar ch
        main_''

-- sequence takes a list of IO actions

mainM = do
    rs <- (sequence $ replicate 3 getLine)
    let rs' = map (\x -> read x :: Int) rs
    print rs'
