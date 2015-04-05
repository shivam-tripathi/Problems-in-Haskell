-- http://www.codechef.com/APRIL15/problems/PIANO1

main :: IO()
main = do
		testCases <- readLn
		sequence $ replicate testCases initialise
		return ()

initialise :: IO()
initialise = do
			str <- getLine
			octaves <- readLn
			print $ solution str octaves

domainKeys :: Int -> Int
domainKeys x = 12 * x

fundamental :: String -> Int
fundamental xs = (sum $ map (\x -> if x == 'T' then 2 else 1) xs) 

patterns :: Int -> Int -> Int -> Int
patterns index dk f = div (dk - index) f

calculation :: Int -> Int -> Int
calculation dk f = sum $ map ( \x -> summation (patterns x dk f )) [1 .. ( f )]

summation x = div (x * (x+1)) 2

solution str octaves = calculation (domainKeys octaves) (fundamental str)