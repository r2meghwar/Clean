module PT5
import StdEnv


/* PT5
	Given a 2-d list of Integer type and a boolean function.
	Transform every integers of the sublist, (that has the returned value True by the given function), into a single-element list.
	for example: [[2,3,4,2,2],[8,5,5,6,7,8],[9,3,7,8,9]] isOdd
				The first sublist will result [3]
				The second sublist will result [5],[5],[7]
				The third sublist will result [9],[3],[7],[9]
				And combining all those result into the list, then
				the result will be, [[3],[5],[5],[7],[9],[3],[7],[9]]
				
*/


functionAux :: [Int] (Int -> Bool) -> [Int]
functionAux [] _ = []
functionAux [x:xs] function
| function x = [x : functionAux xs function]
= functionAux xs function


PT5 :: [[Int]] (Int -> Bool) -> [[Int]]
PT5 [] function = []
PT5 [x:xs] function = [[x] \\ x <- (flatten [functionAux x function : PT5 xs function])]


//Start = PT5 [[2,3,4,2,2],[8,5,5,6,7,8],[9,3,7,8,9]] isOdd 				// [[3],[5],[5],[7],[9],[3],[7],[9]]
//Start = PT5 [[6,5,3,-9],[0,9,-1,8,-4,-7],[8,-5,4,9,-3,0,9,5,7]] ((>)0) 	// [[-9],[-1],[-4],[-7],[-5],[-3]]
//Start = PT5 [[6,0,0,-9],[0,9,-1,0,-4,-7],[-5,0,9,-3,0,0,5]] ((==)0) 		// [[0],[0],[0],[0],[0],[0],[0]]
//Start = PT5 [[],[4,7,8]] isEven 											// [[4],[8]]
//Start = PT5 [] ((<)0) 													// []