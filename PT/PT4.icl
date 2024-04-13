module PT4
import StdEnv

/* PT4
	Given a function and a 2-D list of integer.
	Return the 2-D list by applying the given function 
	to each sublist which doesn't contain the negative number.
	
	(meaning: if the sublist contains a negative number, 
			  don't consider to apply the given function on that sublist and
			  don't take it into the output as well).
			  
	Example: 
	
	doubleAll and [[4,3,2,5,4,3],[1,0,9,0,7,3,2],[4,7,9,-4], [-8,6,1]]
	
	result => [doubleAll [4,3,2,5,4,3], doubleAll [1,0,9,0,7,3,2]] 
		   => [[8,6,4,10,8,6],[2,0,18,0,14,6,4]]
		   
		      [4,7,9,-4] and [-8,6,11] are not considered as they contains a negative number.

*/
// don't delete these functions
doubleAll ls = map (\x = x*2) ls
trueFalse ls = [ result index \\ x <- ls & index <- [0..] ] where result index | isEven index = True = False


isPositive :: [Int] -> Bool
isPositive [] = True
isPositive [x:xs]
| x >= 0 = isPositive xs
= False


oneToAll :: ([Int] -> [a]) [[Int]] -> [[a]]
oneToAll _ [] = []
oneToAll function lists = [ function x \\ x <- lists | isPositive x]


//Start = oneToAll doubleAll [[4,3,2,5,4,3],[1,0,9,0,7,3,2],[4,7,9,-4], [-8,6,1]] // [[8,6,4,10,8,6],[2,0,18,0,14,6,4]]
//Start = oneToAll trueFalse [[4,3,2,5,4,-3],[1,0,-9,0,7,3,2],[4,7,9,4], [8,6,1]] // [[True,False,True,False],[True,False,True]]















