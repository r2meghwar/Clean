module PT6
import StdEnv

/*
	Given a list of integer and return the list of tuple (integer, its divisor:Integer).
	No need to consider negative number.
	For ex:[15,12] then the divisors of 15 are 1,3,5, so (15,1),(15,3),(15,5) and 
							divisors of 12 are 1,2,3,4,6 so (12,1),(12,2),(12,3),(12,4),(12,6)
							
	The result is [(15,1),(15,3),(15,5),(12,1),(12,2),(12,3),(12,4),(12,6)]
*/


divisors :: Int -> [Int]
divisors n = sort [x \\ x <- [1..n/2] | n rem x == 0]

PT6 :: [Int] -> [(Int,Int)]
PT6 list = [(x, y) \\ x <- list, y <- divisors x ] 


//Start = PT6 [15,12]		//[(15,1),(15,3),(15,5),(12,1),(12,2),(12,3),(12,4),(12,6)]
//Start = PT6 [10,8] 		// [(10,1),(10,2),(10,5),(8,1),(8,2),(8,4)]
//Start = PT6 []			// []
//Start = PT6 [6,20,18]   	// [(6,1),(6,2),(6,3),(20,1),(20,2),(20,4),(20,5),(20,10),(18,1),(18,2),(18,3),(18,6),(18,9)]
