module PT1
import StdEnv

/* PT1
	Write a function that take 1 integer called x.
	If x is an even number, return the last digit of x.
	otherwise, return x without the last digit.
	
	Example: x is 1234, then x is an even number, so return 4 (returning the last digit).
			 x is 97545, then x is an odd number, so return 9754 (removing the last digit).
			 
	You don't need to take care of the sign.
*/

PT1 :: Int -> Int
PT1 x 
| isEven x = x rem 10
= x/10


//Start = PT1 1234 // 4
//Start = PT1 97545 // 9754
//Start = PT1 -1230 // 0
//Start = PT1 -345  // -34
