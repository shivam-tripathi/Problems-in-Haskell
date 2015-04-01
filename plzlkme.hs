-- www.codechef.com/problems/PLZLYKME

main = do 
		testCases <- readLn
		sequence $ replicate testCases initialise
		return ()

-- l are minimum number of likes needed at end of d days
-- s are initial number of likes
-- c is the number
initialise = do
			str <- getLine
			let [l,d,s,c] = map(\x -> read x :: Float) $ words str
			if checkIfAlive l d s c 
				then putStrLn "ALIVE AND KICKING" 
				else putStrLn "DEAD AND ROTTING"

checkIfAlive l d s c = l <= (s * ((c+1) ** (d-1))) 


