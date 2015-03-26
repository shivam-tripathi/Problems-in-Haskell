main = do 
		num <- readLn
		if num /= 42 
			then do 
				(print num) 
				main
				
			else 
				 return ()