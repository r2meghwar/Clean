module HW1GR1
import StdEnv

// REMEMBER : COPYING OR GIVING A COPY MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW1GR1 ( e.g JohnSmithHW1GR1)
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

Write a function that calculates the volume of a cylinder given 
its radius and height as input. The function should return the volume. 
If either the radius or height is negative, return -1 to indicate an invalid input.

You can consider pi as 3

Example:

Input : 4 5 
Output : 240 


*/

cylinderVolume :: Int Int -> Int
cylinderVolume radius height 
| radius < 0 || height < 0 = -1
= 3 * (radius)^2 * height


//Start = cylinderVolume 4 5 // 240
//Start = cylinderVolume 10 8 // 2400
//Start = cylinderVolume -2 15 // -1

/*
Task 2:


Given two integer inputs a and b (a<b) , calculate the product of each number
between a and b (inclusive).
If any of the inputs is zero or negative,  return -1

Example :

Input : 5 7
Output : 210 ( 5 x 6 x 7 = 210)

*/

productOfNumbers :: Int Int -> Int
productOfNumbers x y 
| x == y = y
= x * productOfNumbers (x+1) y


//Start = productOfNumbers 5 7 // 120
//Start = productOfNumbers 8 15 // 259459200











//Start = productOfNumbers 4 8 // 6720
//Start = productOfNumbers -3 3 // -1
