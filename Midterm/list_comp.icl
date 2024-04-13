module list_comp
import StdEnv

/*2- Count good lists - 10 points
    Given a list of lists of integer numbers, count the good sublists in 
    the given list. A list is considered to be good if the numbers at 
    even positions are even and the numbers at odd positions are prime.    
    Input:  [[2,2,4,5], [2,3,3,5]]
    Output: 1 (Only the [2,2,4,5] sublist is good as the numbers at 
    0th, 2nd (even) positions are even and the numbers at 1st, 3rd (odd) 
    positions are prime.
*/

isPrimeNumber :: Int -> Bool
isPrimeNumber num = length [i \\ i <- [1..num] | num rem i == 0 ] == 2

isGoodList :: [Int] -> Bool
isGoodList list = length [ x \\ x <- list & i <- [0..length list] | if (isEven i) (isEven x) (isPrimeNumber x) ] == length list

count_good_lists :: [[Int]] -> Int
count_good_lists list = length [x \\ x <- list | isGoodList x]

//Start = count_good_lists [[2,2,4,5],[2,3,3,5]] // 1
//Start = count_good_lists [[2,23,22],[2,29,22,5],[1,2,3]] // 2
//Start = count_good_lists [[2,2,4,5],[2,2,6,7,8,11,12,17],[12,23,4]] // 3
//Start = count_good_lists [] // 0



/*5- Passed students - 10 points
    Given a list of tuples and an integer number (let's call it x), where 
    the first element of the tuple represents a student's name and 
    the second element of the tuple represents the points of the student 
    that he/she got in a particular subject (its type is a list of real numbers).
    Return those students whose points have the following property:
    if the sum of the INTEGER parts of the points is greater than or equal 
    to the given number x.
    Input: [("Abdullah",[55.55,66.55,77.75,65.07,65.57]),("Mohammed",[27.55,20.55,10.75,30.07,20.57])] 320 
    Output: ["Abdullah"] ( the sum of the integer parts of [55.55,66.55,77.75,65.07,65.57] 
                           = 55 + 66 + 77 + 65 + 65 = 328 >= 320 (the given x)
                           - the sum of the integer parts of [27.55,20.55,10.75,30.07,20.57]
                           = 27 + 20 + 10 + 30 + 20 = 107 < 320 (the given x) )
*/

toInteger :: Real -> [Int]
toInteger x
| toReal (toInt x) > x = [toInt x - 1] 
= [toInt x]

passedStudents :: [(String,[Real])] Int -> [String]
passedStudents list n = [ name \\ (name,marks) <- list | sum (map sum (map toInteger marks)) >= n ]

//Start = passedStudents [("Abdullah",[55.55,66.55,77.75,65.07,65.57]),("Mohammed",[27.55,20.55,10.75,30.07,20.57])] 320 // ["Abdullah"]
//Start = passedStudents [("Sara" , [5.55,44.55,55.75,30.07,90.57]),("Rayan",[56.55,66.55,7.75,77.07,77.57]),("Ali",[1.55,6.55,66.75,6.07,7.57]),("Maria",[54.55,60.55,66.75,20.07,74.57])] 200 // ["Sara","Rayan","Maria"]
//Start = passedStudents [] 100 // []

// 2. Given a positive integer number. Create a list of lists like:
// [[n,n-1,..1,1,..,n],[n-1,n-2,..1,1,..,n-1]....[2,1,1,2],[1,1],[1,1],[2,1,1,2],.....[n-1,n-2,..1,1,..,n-1],[n,n-1,..1,1,..,n]]
// Example: for 3 the created list is:
// [[3,2,1,1,2,3],[2,1,1,2],[1,1],[1,1],[2,1,1,2],[3,2,1,1,2,3]]

decreasing_list :: Int -> [[Int]]
decreasing_list n = (map (\x = [x,x-1..1] ++ [1,2,x]) [n,n-1..1]) ++ (map (\x = [x,x-1..1] ++ [1,2,x]) (reverse [n,n-1..1]))

//Start = decreasing_list 3 // [[3,2,1,1,2,3],[2,1,1,2],[1,1],[1,1],[2,1,1,2],[3,2,1,1,2,3]]
//Start = decreasing_list 5 // [[5,4,3,2,1,1,2,3,4,5],[4,3,2,1,1,2,3,4],[3,2,1,1,2,3],[2,1,1,2],[1,1],[1,1],[2,1,1,2],[3,2,1,1,2,3],[4,3,2,1,1,2,3,4],[5,4,3,2,1,1,2,3,4,5]]
//Start = decreasing_list 1 // [[1,1],[1,1]]

// 6. Given an integer, return a list that has all the prime factors of the given number.
// The list should contain the prime factors in ascending order.
// Note: we do not consider 1 a prime number.

primeFactors :: Int -> [Int]
primeFactors number = [i \\ i <- [2..number] | number rem i == 0 && isPrimeNumber i]

//Start = primeFactors 0 // []
//Start = primeFactors 1 // []
//Start = primeFactors 17 // [17]
//Start = primeFactors 374 // [2, 11, 17]
//Start = primeFactors 672 // [ 2, 3,  7]
//Start = primeFactors 41533164779// [19, 23,31, 37, 41, 43, 47]

/* 5. Symmetric difference 

 Given two lists of integer numbers , return a sorted list containing the symmetric difference of the two lists; 
 The symmetric difference of two lists A and B is the list (A – B) U (B – A); 
 where A - B is The difference of two lists  defined as follows:  
 The List A-B consists of elements that are in A but not in B.
 And (U) the union of two lists is a list containing all the elements of A and B without duplicates 
*/

symmetricDif :: [Int] [Int] -> [Int]
symmetricDif listA listB = removeDup (a ++ b)
where a = [x \\ x <- listA | (isMember x listA) && not (isMember x listB) ]
      b = [y \\ y <- listB | not (isMember y listA) && (isMember y listB) ]
      
//Start = symmetricDif  [1,2,3,4,5] [2,4,6] //  [1,3,5,6]
//Start = symmetricDif  [1..5] [1..10] // [6,7,8,9,10]
//Start = symmetricDif  [1..5] [] // [1,2,3,4,5]

/* 6. zipWith

 Implement the function zipWith that takes a function, 
 and two lists, and combines them in such a way that 
 elements that are in the same positions get the function 
 applied to them.

 E.g: zipWith addTwoNumbers [1,2,3] [5,6,7] = [1+5,2+6,3+7] = [6,8,10]
*/
//DON'T DELETE THESE FUNCTIONS !!!
addTwoNumber x y = x + y
prodTwoNumber x y = x * y
niceTwoNumber x y = x rem y
//

zipWith :: (Int Int -> Int) [Int] [Int] -> [Int]
zipWith function listA listB = [function x y \\ x <- listA & y <- listB ]

//Start = zipWith addTwoNumber [1,2,3] [5,6,7] // [6,8,10]
//Start = zipWith prodTwoNumber [1,2,3] [5,6,7] // [5,12,21]
//Start = zipWith niceTwoNumber [5,6,7] [1,2,3] // [0,0,1]


/* 10. Twin primes
 
 Twin primes is a pair of primes, such that it contains a prime number that is either 
 2 less or 2 more than the pair prime number.
 For example, (41, 43) is a twin prime pair.
 Given a range of numbers left..right write a function that returns the count of 
 twin primes within the range.

 E.g: between 1 and 50 there are 6 pairs of twin prime numbers:
 [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43)].
*/

twinPrimes :: Int Int -> Int
twinPrimes a b = length [(x,y) \\ x <- [a..b] & y <- [(a+2)..b] | isPrimeNumber x && isPrimeNumber y]

//Start = twinPrimes 1 50 // 6
//Start = twinPrimes 1 1000 // 35
//Start = twinPrimes 0 2 // 0
//Start = twinPrimes 0 -5 // 0





