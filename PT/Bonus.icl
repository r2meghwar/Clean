module Bonus
import StdEnv

/*
 1. (bonus point - 50p) rewrite map using foldr (50p)
*/

mymap :: (a -> b) [a] -> [b]
mymap func ls = foldr (\x xs = [func x : xs]) [] ls

//Start = mymap inc [1..10]

/*
2. (bonus point - 50p) Compute the average of a list of float point numbers using the foldr function
 in one line code using one lambda function.
*/

avg :: [Real] -> Real
avg list = foldr (+) 0.0 ([x \\ x <- list ]) / toReal (length list)

//Start = avg [16.2, 17.8, 11.5] // 15.1666666666667
//Start = avg [13.0, 40.9] // 26.95

/*
3. (bonus point - 50p) Write a function that takes a list of numbers and adds the first element,
 subtracts the second element, adds the third element, subtracts the fourth element, so on, 
 in this alternating repetition. 
 For example: [2,3,4,5,6,7] -> 2-3+4-5+6-7 = -3
*/

alternatingSum :: [Int] -> Int
alternatingSum list = sum [x* (-1)^i \\ x <- list & i <- [0 .. length list]]

//Start = alternatingSum [2..7] //-3
//Start = alternatingSum [45,-5,63,46,-345,4321] //-4599
//Start = alternatingSum [] //0