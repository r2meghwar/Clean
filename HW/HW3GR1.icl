module HW3GR1
import StdEnv


// REMEMBER : COPYING OR GIVING A COPY MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW3GR1 ( e.g JohnSmithHW3GR1)
Also, don't forget to change filename in the first line of file  */

//Please write your neptun code here: 

/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/




/*

Task 1:

You are given a list of lists, where each inner list contains numbers as integers. 
Create a function that filters out the inner lists containing any even number 
and then merges the remaining lists into a single list, removing any duplicates. 

Example :
	input : [[1, 3, 5, 7], [2, 4, 6, 8], [9, 0, 1, 3],[3,7,11,13,15]] 
	output : [1,3,5,7,11,13,15]
	
Explanation :
	second and third inner list of outer list contains even number so we eliminate them.
	[[1,3,5,7],[3,7,11,13,15]] remains. Then we merge them into single list
	 and eliminate any duplicates : [1,3,5,7,11,13,15]

Hint : you can use built-in functions like filter, flatten
*/

isContainAnyEven :: [Int] -> Bool
isContainAnyEven [] = True
isContainAnyEven [x:xs]
| isEven x || x == 0 = False
= isContainAnyEven xs

filterAndMergeNumbers :: [[Int]] -> [Int]
filterAndMergeNumbers lists = removeDup (flatten [x \\ x <- lists | isContainAnyEven x ])


//Start = filterAndMergeNumbers [[1, 3, 5, 7], [2, 4, 6, 8], [9, 0, 1, 3],[3,7,11,13,15]] // [1,3,5,7,11,13,15]
//Start = filterAndMergeNumbers [[2, 4, 6, 8], [10, 12, 14], [3, 5, 7], [15, 15, 17 ,19]] // [3,5,7,15,17,19]

/*
Task 2:

You are given a list of integers. Your task is to remove the element that 
occurs the most in the list. If there are multiple elements with 
the same highest frequency, remove all of them

Example :
	input : [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
	output : [1,2,2,3,3,3]
	
Explanation :
	in this list, 4 is the most frequent element in the list. It occurs 4 times. 
	We remove it and remaining list is like this : [1,2,2,3,3,3]
*/


Occurrence :: Int [Int] -> Int
Occurrence _ [] = 0
Occurrence n [x:xs]
| n == x = 1 + Occurrence n xs
= Occurrence n xs


OccurrenceList :: [Int] [Int] -> [Int]
OccurrenceList [] _ = []
OccurrenceList [x:xs] list = [Occurrence x list : OccurrenceList xs list]


removeMostCommonElementAux :: [Int] [Int] -> [Int]
removeMostCommonElementAux [] list = []
removeMostCommonElementAux [x:xs] list 
| Occurrence x list == maxList (OccurrenceList list list) = removeMostCommonElementAux xs list
= [x: removeMostCommonElementAux xs list]


removeMostCommonElement :: [Int] -> [Int]
removeMostCommonElement list = removeMostCommonElementAux list list


//Start = removeMostCommonElement [1, 2, 2, 3, 3, 3, 4, 4, 4, 4] // [1,2,2,3,3,3]
//Start = removeMostCommonElement [5, 5, 5, 6, 6, 7, 7, 8, 8, 8, 9, 9, 9, 9, 10] // [5, 5, 5, 6, 6, 7, 7, 8, 8, 8, 10]
//Start = removeMostCommonElement [3,4,4,4,5,5,6,6,6,7,8,9] // [3,5,5,7,8,9]









