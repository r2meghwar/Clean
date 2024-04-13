module HW5GR1

import StdEnv

// REMEMBER : COPYING OR GIVING A COPY MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW5GR1 ( e.g JohnSmithHW5GR1)
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
1. Given a list of lists, create a list of tuples that contains the index, 
product of the maximum and minimum elements in the list, and a boolean value that 
checks if the elements in the list are in  not increasing order( anything except strictly increasing).

EXAMPLE:

Input : [[1, 2, 3], [4, 5], [6, 1, 8], [7, 3, 5, 2], []]
Output : [(0, 3, False), (1, 20, False), (2, 8, True), (3, 14, True),(4,0,True)]

EXPLANATION  : index of the first sublist is 0, minimum element is 1 ,
maximum element is 3 so their product is 3, and this list is 
strictly increasing so we put False. Others follow the same logic
*/


strictlyIncreasing :: [Int] -> Bool
strictlyIncreasing [] = True
strictlyIncreasing [x] = False
strictlyIncreasing [x,n:xs]
| x <= n = strictlyIncreasing xs
= True

maximum :: [Int] -> Int
maximum [] = 0
maximum list = maxList list

minimum :: [Int] -> Int
minimum [] = 0
minimum list = minList list

checkProdAndOrder :: [[Int]] -> [(Int,Int,Bool)]
checkProdAndOrder lists = [ (i , maximum x * minimum x, strictlyIncreasing x) \\ x <- lists & i <- [0..length lists-1] ]


//Start = checkProdAndOrder [[1, 2, 3], [4, 5, 5], [6, 1, 8], [7, 3, 5, 2], []] // [(0, 3, False), (1, 20, False), (2, 8, True), (3, 14, True),(4,0,True)]
//Start = checkProdAndOrder [[2, 4, 6], [1, 3, 5], [10, 20, 30], [3, 2, 1]] // [(0, 12, False), (1, 5, False), (2, 300, False), (3, 3, True)]
//Start = checkProdAndOrder [[5, 10, 15], [0, 0, 0], [1, 2, 3, 5, 4]] // [(0, 75, False), (1, 0, True), (2, 5, True)]


/*
2. You are given a list of characters. Your task is to split this list into a tuple with three parts:

1. The first part should contain all the lowercase letters (a to z) in the same order 
they appear in the original list.
2. The second part should contain all the uppercase letters (A to Z) 
in the same order they appear in the original list.
3. The third part should contain all the remaining characters 
(digits, special characters, etc.) in the same order they appear in the original list.

EXAMPLE:

Input: ['H', 'e', '1', 'l', '2', 'l', '3', 'o', '4', '!', 'W', 'o', 'r', '5', 'l', 'd']
Output: (['e', 'l', 'l', 'o', 'o', 'r', 'l', 'd'], ['H', 'W'], ['1', '2', '3', '4', '5', '!'])
*/

splitChars :: [Char] -> ([Char],[Char],[Char])
splitChars chars =
	  let   lower = filter isLower chars
      		upper = filter isUpper chars
      		other = filter (\x -> not (isLower x || isUpper x)) chars
      in (lower, upper, other)

//Start = splitChars ['H', 'e', '1', 'l', '2', 'l', '3', 'o', '4', '!', 'W', 'o', 'r', '5', 'l', 'd'] //  (['e', 'l', 'l', 'o', 'o', 'r', 'l', 'd'], ['H', 'W'], ['1', '2', '3', '4', '5', '!'])
//Start = splitChars ['A', 'B', 'C', '1', '2', '3', '*', 'a', 'b', 'c', 'd', 'e'] // (['a', 'b', 'c', 'd', 'e'], ['A', 'B', 'C'], ['1', '2', '3', '*'])
//Start = splitChars ['X', 'Y', 'Z', '9', '8', '7', '6', '5', '=', 'a', 'b', 'c'] // (['a', 'b', 'c'], ['X', 'Y', 'Z'], ['9', '8', '7', '6', '5', '='])


