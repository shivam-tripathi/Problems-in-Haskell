import Data.Char
import Control.Monad
import System.IO

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
-- For multi-line input use sequence along with replicate.
-- Replicate can create an array of IO functions.
mainM = do
    rs <- (sequence $ replicate 3 getLine)
    let rs' = map (\x -> read x :: Int) rs
    print rs'

-- mapping print with a list will not give the print action execution.
-- This is as it will return a list of IO actions. However, sequence can take
-- a list of IO action and execute them in order. However, with the execution of
-- IO action print/putStrLn, we get a list of tuples from the IO mapping as
-- return of mapping. It is the result of mapping.
-- To overcome this, mapM or mapM_ can be used.
-- However, if the printing is done in an IO fuction i.e. inside do block, the
-- result of IO function if not binded, list of tuples will not occur on
-- the screen (when sequence is used) IF the last statement is not sequence one.

-- Single tuple if returned, is not printed. A list however is.

mainM_ = do
    let rs = [1,2,3,4,5]
    sequence $ map print rs
    mapM print rs
    mapM_ print rs

-- forever in Control.Monad repeats an IO function forever
capslocked = forever $ do
    putStrLn "Input a line:"
    line <- getLine
    putStrLn (map toUpper line)

-- forM in Control.Monad takes a list, maps an IO function over it, and applies
-- sequence over it.

forMcheck = do
    colors <- forM [1 .. 4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        getLine)
    forM [1 .. 4] (\a -> do
        putStrLn $ "Color for number " ++ show a ++ " is : " ++  (colors!!(a-1)))
    return ()

-- mapM doesnot return the value being returned by the IO function? Here I
-- explicitly return color, even then?
-- For IO fucntion print it returns list of tuples.
mapM_check = do
    colors <- mapM_ (\a -> do
        color <- getLine
        return color) [1 .. 4]
    print colors

-- :t getContents
-- getContents :: IO String

-- haiku ::
-- I am a lil' teapot
-- What's with the airplane food, huh?
-- It's so small, tasteless

-- Piping in Linux
-- "cat _fileName_ | ./_executable_"
-- Another similar method is " ./_executable_ < _fileName_ "

getContentsFunc = do
    line <- getContents
    print line
    let line' = lines line
    forM line' (\a -> do
        if (length a <10) then print a else return ())
    return ()
-- Could have uses unlines and putStrLn

-- interact function
-- It takes input from the terminal/file like getContents and passes it to said
-- function till EOF is not encountered. It will print the result on the
-- terminal.
interactCheck = interact shortLinesOnly

shortLinesOnly input = let
    shortLines = filter
        (\a -> if length a < 10 then True else False) (lines input)
    result = unlines shortLines
    in result

-- This function must be motivation for all function composition.
interactCheck2 = interact $ unlines . filter ((<10).length) . lines

isPalindrome = interact $ unlines.map
    (\a -> if (a == reverse a) then "Palindrome" else "not palindrome").lines

-- Input from files is a lazy process. It is carried out line by line, and a
-- line is input only when it is needed.

fileInp = do
    handle <- openFile "haiku" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- :t openFile
-- openFile :: FilePath -> IOMode -> IO Handle
-- Filename is type synonym for String.
-- IOMode type has following value contructors:
--   ReadMode
--   WriteMode
--   AppendMode
--   ReadWriteMode

