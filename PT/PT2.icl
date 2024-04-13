module PT2
import StdEnv

/* PT2
	Given an integer N (N > 1), find the product of all odd numbers between 1..N.
*/

PT2 :: Int -> Int
PT2 1 = 1
PT2 x 
| isOdd x = x * PT2 (x-1)
= PT2 (x-1)


//Start = PT2 3	// 3
//Start = PT2 7  // 7*5*3*1 = 105
//Start = PT2 10 // 9*7*5*3*1 = 945
//Start = PT2 12   // 10395

