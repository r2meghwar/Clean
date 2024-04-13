module HW2GR1
import StdEnv

// REMEMBER : COPYING OR GIVING A COPY MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW2GR1 ( e.g JohnSmithHW1GR1)
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

Implement a recursive function countPalindromicNumbers that accepts
an integer n as input and computes the count of 
palindromic numbers within the range from 1 to n. 
A palindromic number is a number that reads the same forwards and backwards. 
For instance, numbers like 121 and 1331 are considered palindromic numbers.
Hint: you may need to use auxiliary function(s)
*/

reverseNumber :: Int Int -> Int
reverseNumber 0 rev = rev
reverseNumber x rev = reverseNumber (x/10) (x rem 10 + (rev*10))


countPalindromicNumbersAux :: Int Int -> Int
countPalindromicNumbersAux 0 counter = counter
countPalindromicNumbersAux n counter
| n == reverseNumber n 0 = countPalindromicNumbersAux (n-1) (counter+1)
= countPalindromicNumbersAux (n-1) counter


countPalindromicNumbers :: Int -> Int
countPalindromicNumbers n = countPalindromicNumbersAux n 0


//Start = countPalindromicNumbers 100 // 18
//Start = countPalindromicNumbers 8 // 8
//Start = countPalindromicNumbers 1000 // 108


/*
Task 2:

You are given 2 lists of integer. Your task is to add the integer in the same position from both list. 
If there are extra elements in one of the list, then append all that extra elements into the result.

For ex1: [1,2,3,4,5] and [3,2,5,6,7]
		The result list is [1+3, 2+2, 3+5, 4+6, 5+7] => [4,4,8,10,12]
		
	ex2: [1,2,3,4,5,8,9] and [3,2,5,6,7]
			
			[1,2,3,4,5,8,9]
		  	[3,2,5,6,7]
		  	-----------------
			[4,4,8,10,12,8,9]
		The result list is => [4,4,8,10,12,8,9]
		
	ex3: [1,2,3,4,5] and [3,2,5,6,7,8,9]
			
			[1,2,3,4,5]
		  	[3,2,5,6,7,8,9]
		  	-----------------
			[4,4,8,10,12,8,9]
		The result list is => [4,4,8,10,12,8,9]
		
*/

addTwoList :: [Int] [Int] -> [Int]
addTwoList [] [] = []
addTwoList [] [y:ys] = [y:ys]
addTwoList [x:xs] [] = [x:xs]
addTwoList [x:xs] [y:ys] = [x+y : addTwoList xs ys]

//Start = addTwoList [1,2,3,4,5] [3,2,5,6,7] // [4,4,8,10,12]
//Start = addTwoList [1,2,3,4,5,8,9] [3,2,5,6,7] // [4,4,8,10,12,8,9]
//Start = addTwoList [1,2,3,4,5] [3,2,5,6,7,2,3] // [4,4,8,10,12,2,3]
//Start = addTwoList [] [2,3] // [2,3]
//Start = addTwoList [] [] // []



