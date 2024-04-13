module PT3
import StdEnv

/* PT3

	Given a 2-D list of integer, remove all the sublist that has the summation lower than 0.
	
	Example: 			[[1,-2,3,-4], [1,2,3,4],[-1,-2,-3,4,5]]
	sum of each sublist:     -2           10          3
	
	Result: [[1,2,3,4],[-1,-2,-3,4,5]]
	
	If the sublist is empty, then the summation is 0, so no need to remove that empty sublist.
*/

removeNegativeSum :: [[Int]] -> [[Int]]
removeNegativeSum lists = [x \\ x <- lists | sum x >= 0]


//Start = removeNegativeSum [[1,-2,3,-4], [1,2,3,4],[-1,-2,-3,4,5]] // [[1,2,3,4],[-1,-2,-3,4,5]]
//Start = removeNegativeSum [[-100,200,-200],[], [-1,2]]	// [[],[-1,2]]
//Start = removeNegativeSum [[100,200,-200],[], [-1,-2]]	// [[100,200,-200],[]]
//Start = removeNegativeSum [] // []

