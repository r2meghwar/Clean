module HW4GR1

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
Task 1 : 
	Given a list of three functions and a 2-dimensional list of integers, 
	apply the functions to the sublists based on the sum of their elements. 
	If the sum of the elements in a sublist is positive, apply the first function. 
	If the sum is negative, apply the second function. 
	If the sum is zero, apply the third function.
	
	[func1, func2, func3]  [[1,2,-5,-2], [1, 2, 3, 4] [6, -4, 3, -5]]
	Result -> [func2 [1,2,-5,-2], func1 [1, 2, 3, 4], func3 [6, -4, 3, -5] ]
*/

zeroAll ls = map (\x = 0) ls
oneAll ls = map (\x = 1) ls
doubleAll ls = map (\x = x*2) ls
tripleAll ls = map (\x = x*3) ls
halfAll ls = map (\x = x/2) ls
sqAll ls = map (\x = x^2) ls

falseAll ls = map (\x = False) ls
trueAll ls = map (\x = True) ls
trueFalse ls = [ result index \\ x <- ls & index <- [0..] ] where result index | isEven index = True = False

zeroStrAll ls = map (\x = "0") ls
oneStrAll ls = map (\x = "1") ls
falseStrAll ls = map (\x = "False") ls
trueStrAll ls = map (\x = "True") ls


applyFunctionsAux :: [([Int] -> [a])] [Int] -> [a]
applyFunctionsAux function list
| sum list > 0 = (function !! 0) list
| sum list < 0 = (function !! 1) list
| sum list == 0 = (function !! 2) list


applyFunctions :: [([Int] -> [a])] [[Int]] -> [[a]]
applyFunctions _ [] = []
applyFunctions function [x:xs] = [applyFunctionsAux function x: applyFunctions function xs]


//Start = applyFunctions [doubleAll, tripleAll, halfAll] [[1,2,-5,-2], [1, 2, 3, 4], [6, -4, 3, -5]] // [[3, 6, -15, -6], [2, 4, 6, 8], [3, -2, 1, -2]]
//Start = applyFunctions [zeroAll, oneAll, doubleAll] [[2, 3, -9, 2], [-4, -3, 5, 1], [11, 13, -10, -5]] // [[1, 1, 1, 1], [1, 1, 1, 1], [0, 0, 0, 0]]



/*

Task 2 : 

You are given a list of numbers.

1. If a number is greater than 10, replace it with its square.

2. In the updated list from step 1, if a number is divisible by 4, divide it by 4.

3 .From the list obtained in step 2, remove numbers that are multiples of 6.

Example:
INPUT : [16, 25, 7, 36, 10, 49, 100]

Step 1: If a number is greater than 10, replace it with its square.
Updated list : [256,625,7,1296,10,2401,10000]

Step 2: In the updated list from step 1, if a number is divisible by 4, divide it by 4.
Updated list : [64,625,7,324,10,2401,2500]

Step 3: From the list obtained in step 2, remove numbers that are multiples of 6.

Final list: [64,625,7,10,2401,2500]
*/

processNumbers1 :: [Int] -> [Int]
processNumbers1 [] = []
processNumbers1 [x:xs]
| x > 10 = [x^2 : processNumbers1 xs]
= [x: processNumbers1 xs] 

processNumbers2 :: [Int] -> [Int]
processNumbers2 [] = []
processNumbers2 [x:xs]
| x rem 4 == 0 = [x/4 : processNumbers2 xs]
= [x: processNumbers2 xs] 


processNumbers3 :: [Int] -> [Int]
processNumbers3 [] = []
processNumbers3 [x:xs]
| x rem 6 == 0 = processNumbers3 xs
= [x: processNumbers3 xs] 


processNumbers :: [Int] -> [Int]
processNumbers list = processNumbers3 (processNumbers2 (processNumbers1 list))


//Start = processNumbers [16, 25, 7, 36, 10, 49, 100] // [64,625,7,10,2401,2500]
//Start = processNumbers [6, 14, 28, 35, 42, 49, 70] // [49,196,1225,441,2401,1225]
//Start = processNumbers [4, 9, 12, 25, 36, 81, 100, 121] // [1,9,625,6561,2500,14641]






