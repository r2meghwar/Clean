module HW6GR1
import StdEnv

// REMEMBER : COPYING OR GIVING A COPY MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW6GR1 ( e.g JohnSmithHW5GR1)
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
1. You are given a list of tuples of integers, where each tuple is in the form (a, b, c). 
For each tuple, if the sum of the first two elements (a + b) is not greater 
than the third element (c), eliminate that tuple from the list. 
If the sum (a + b) is greater than the third 
element (c), repeat that tuple in the output list a + b - c times.

Example:
Input: [(1, 3, 5), (2, 7, 8), (4, 2, 6), (9, 6, 10), (5, 5, 10)]
Output : [(2, 7, 8), (9, 6, 10), (9, 6, 10), (9, 6, 10), (9, 6, 10), (9, 6, 10)]

explanation: (1, 3, 5),(4, 2, 6),(5, 5, 10) are eliminated from list because
a+b is smaller or equal to c. (2,7,8) is repeated 1 times because 2+7-8 =1, 
(9,6,10) is repeated 5 times because 9+6-10 = 5.

*/

repeatN :: (Int,Int,Int) Int -> [(Int,Int,Int)]
repeatN _ 0 = []
repeatN tuple n = [tuple: repeatN tuple (n-1)] 

eliminateAndRepeat :: [(Int,Int,Int)] -> [(Int,Int,Int)]
eliminateAndRepeat list = flatten [repeatN (a,b,c) (a+b-c) \\ (a,b,c) <- list | a+b > c ]

//Start  = eliminateAndRepeat [(1, 3, 5), (2, 7, 8), (4, 2, 6), (9, 6, 10), (5, 5, 10)] 
// [(2, 7, 8), (9, 6, 10), (9, 6, 10), (9, 6, 10), (9, 6, 10), (9, 6, 10)]
//Start = eliminateAndRepeat [(3, 4, 7), (5, 6, 10), (2, 2, 3), (8, 5, 11), (7, 7, 13)] 
// [(5,6,10),(2,2,3),(8,5,11),(8,5,11),(7,7,13)]
//Start = eliminateAndRepeat [(2, 2, 4), (3, 5, 9), (7, 6, 13), (8, 6, 12)] 
// [(8,6,12),(8,6,12)]
//Start = eliminateAndRepeat [(1, 1, 3), (2, 2, 5), (3, 4, 8), (4, 3, 7)] // []

/*
2. You are given a string consisting of words separated by spaces. 
Your task is to encrypt each word in the string by 
shifting the letters of each word forward by a fixed number of positions. 
The number of positions to shift is provided as an integer.

For example:

Given the string "hello world" with the integer 3, return "khoor zruog."
Explanation:

Shift each letter in "hello" by 3 positions to get "khoor"
Shift each letter in "world" by 3 positions to get "zruog"
*/

Aux :: [Char] Int -> String
Aux list n = toString [if (isLower x || x == ' ') (x) (toChar (toInt x - 26 )) \\ x <- list ]

encryptAux :: Char Int -> Char
encryptAux char n
| char == ' ' = ' '
= toChar (toInt char + n)

encrypt :: String Int -> String
encrypt str n = Aux [ (encryptAux x n) \\ x <-: str] (n)

//Start = encrypt "hello world" 3 // "khoor zruog"
//Start = encrypt "encrypt this" 2 // "gpetarv vjku"
//Start = encrypt "programming is awesome" 5 // "uwtlwfrrnsl nx fbjxtrj"
//Start = encrypt "lazy zebra" 3 // "odcb cheud"






