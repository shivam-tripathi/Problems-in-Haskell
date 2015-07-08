-- Implementation of English Ruler.

draw_Tick :: Int -> String
draw_Tick x = replicate x '-'

pattern :: Int -> Int -> Int -> [Int]
pattern a b n = if n == 0 then [a,b] else
    let (xs, (y:ys)) = ((pattern a n (n-1)),(pattern n b (n-1))) in xs++ys

ans size = pattern size size (size - 1)

main = do
    putStrLn "Input size of major tick and number of inches:"
    sizeString <- getLine
    inchesString <- getLine
    let size = read sizeString :: Int
    let inches = read inchesString :: Int
        arr = concat $ replicate  inches (init.map draw_Tick $ (ans size))
    mapM_ putStrLn  ( arr ++ [draw_Tick size] )
