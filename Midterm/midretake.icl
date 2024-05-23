module midretake
import StdEnv


/* Write <Name> and <Neptun Id> here.
by this YOU DECLARE this file is 
YOUR OWN SOLUTION for Functional 
Programming midterm retake, 2024 May 23.
If name and neptun missing, 
we will not check the file!!!*/



/*1. Almost prime
A prime number is a positive integer which has exactly two divisors. 
We say a number is "almost-prime" if it has exactly three divisors.
Given a natural number, check if it has exactly 3 divisors.
Input: 9 Output: True Explanation: 1, 3, 9
Input: 2 Output: False Explanation: 1, 2
Input: 18 Output: False Explanation: 1, 2, 3, 6, 9, 18
*/

isTPrime :: Int -> Bool
isTPrime n = length [ div \\ div <- [1..n] | n rem div == 0 ] == 3

//Start = isTPrime 1 	// False
//Start = isTPrime 2 	// False
//Start = isTPrime 3 	// False
//Start = isTPrime 4 	// True
//Start = isTPrime 20 	// False
//Start = isTPrime 289 	// True



/*2. Convert
Write a function that takes a number a number and turns it into a 
string in the following way (for simplicity return all lowercase)
1 becomes a
2 becomes b
..
9 becomes i
0 becomes _
Hardcoded solutions are not accepted!
*/

numToList :: Int -> [Int]
numToList 0 = []
numToList n = numToList (n/10) ++ [n rem 10]

toCharFunc :: Int -> Char
toCharFunc 0 = '_'
toCharFunc n = toChar (n+96)

f1 :: Int -> String
f1 n = { toCharFunc el \\ el <- (numToList n)}

//Start = f1 100234 // "a__bcd"
//Start = f1 10101010 // "a_a_a_a_"
//Start = f1 246810 //"bdfha_"
//Start = f1 9876543210 // "ihgfedcba_"



/*3. Sum
Write a function that takes a list of integers and returns the 
sum of numbers between the first negative and the first zero 
(including the first negative). If there's no 0 after first 
negative, sum until the end of the list.
Eg. [1,2,3,-3,4,5,6,0,-6,0,5,-7]
numbers between first negative and first zero -3,4,5,6 
their sum is 12
*/

f3 :: [Int] -> Int
f3 list = sum (takeWhile ((<>)0) (dropWhile ((<)0) list))

//Start = f3 [1,2,3,-3,4,5,6,0,-6,0,5,-7] // 12
//Start = f3 [1,4,5] // 0
//Start = f3 [5,5,-2,4,5] // 7
//Start = f3 [1,1,1,-1,1,2,3,4,5,0,-1,10,0] // 14



/* 4. Followed by
Given a list of integers and an integer, calculate how many 
times that number is immediately followed by an even number.
Eg.: list = [3, 4, 5, 2, 3, 5, 3, 8], nr = 3 
output: 2, after 3 an even number appears twice (4 and 8)
*/

followedByEvenAux :: Int Int Int -> Int
followedByEvenAux el y n
| el == n && isEven y = 1
= 0

followedByEven :: [Int] Int -> Int
followedByEven list n = sum[followedByEvenAux el y n\\el<-list & y<-(tl list)]

//Start = followedByEven [3, 4, 5, 2, 3, 5, 3, 8] 3 // 2
//Start = followedByEven [1,6,3,6,6,4,9] 6 // 2
//Start = followedByEven [1,2,5,3,4,5,7,5] 5 // 0



/*5. Sum2
You are given a list of integers and a number. Find in the list 
all the pairs of numbers that add up to the given number.
Eg. [1..5] 5 -> [(1,4),(2,3)] 
(1,4) and (4,1) counts the same so only (1,4) in result
*/

sum_two :: [Int] Int -> [(Int,Int)]
sum_two list n = [(x,y)\\x<-ls&y<-(tl ls)|x+y==n]
where ls = removeDup(flatten[[el,y]\\el<-list,y<-(tl list)|el+y==n])

//Start = sum_two [1..5] 5 // [(1,4),(2,3)]
//Start = sum_two [1..5] 3 // [(1,2)]
//Start = sum_two [5, 8, 32, 7, 2, 6, 9, 12, 52, 3] 9 // [(7,2),(6,3)]
//Start = sum_two [1..9] 10 // [(1,9),(2,8),(3,7),(4,6)]



/*6. Backspace
Assume "#" is a backspace, meaning the string "a#bc#d" actually is "bd",
all letters that come before the '#' sign are deleted.
Your task is to process a list of characters with '#' signs and return 
the formed string. It should not contain '#' signs or any letter that 
is followed directly by a '#' sign.

Examples: 
['a','b','c','#','d','#','#','c']      ==>  "abc"  // c is the last one
['a','b','c','#','#','d','#','#','#']  ==>  "ab"
['#','#','#','#','#','#']       ==>  ""
[]              ==>  ""
*/

finalfunc :: [Char] -> [Char]
finalfunc [] = []
finalfunc [x:xs]
| x == '#' || x == ' ' = finalfunc xs
= [x] ++ finalfunc xs

Aux :: Char -> [Char]
Aux char
| char <> '#' = [char]
= []

backSpaceAux :: Char Char -> [Char]
backSpaceAux a b
| b == '#' = [' ']
= [a]

backSpace :: [Char] -> String
backSpace [] = ""
backSpace list = {el\\el<-(finalfunc ((flatten[backSpaceAux el y\\el<-list&y<-(tl list)]) ++ (Aux (last list))))}

//Start = backSpace ['a','b','c','#','d','#','#','c']  // "abc"
//Start = backSpace ['a','b','c','#','#','d','#','#','#'] // "ab"
//Start = backSpace ['a','b','#','x', 'y', '#','z','#','w','y','q','v','#','#','z','u'] // "axwyqzu"
//Start = backSpace ['#','#','#','#','#','#'] // ""
//Start = backSpace [] // ""



/*7. Filter and add
Given a list of integers, first, filter out any integer that is divisible by 6. 
For the remaining integers, add the sum of its digits to integers themselves.
Finally, return the modified list of integers.
Example: from [12, 22, 9, 6, 18] we filter out 12, 6 and 18 
because they are divisible by 6. Remaining list is [22,9]. 
We add sum of digits of integers to integers themselves. 
22's sum of digits is 4 (2+2), and 9's sum of digits is 9. 
So final list becomes [26,18].
*/

filterThenAdd :: [Int] -> [Int]
filterThenAdd list = [el+sum[x\\x<-(numToList el)]\\el<-(filter (\x=x rem 6<>0) list)]

//Start = filterThenAdd [12,22,9,6,18] // [26,18]
//Start = filterThenAdd [11, 25, 24, 47, 59] // [13, 32, 58, 73]
//Start = filterThenAdd [6, 12, 18, 24, 36] // []



/*8. Words
You are given list of words as a list of characters. Output those words
that have just 2 vowels 'a' and 'e', and 'e' comes after 'a'. 
Convert these into string.
Eg. the word "baker" has only 2 vowel - 'a' and 'e'
'a' before after 'e'
*/

EComesAfterA :: [Char] -> Bool
EComesAfterA list
| isMember 'e' list && isMember 'a' list && length (takeWhile ((<>)'a') list) < length (takeWhile ((<>)'e') list) = True
= False

crosswordWords :: [[Char]] -> [String]
crosswordWords lists = [{x\\x<-el}\\el<-lists|EComesAfterA el]

//Start = crosswordWords [['b','a','b','e','l'],['l','a','s','e','r'],['b','o','o','k']] // ["babel", "laser"]
//Start = crosswordWords [['e','a','r','l','y'],['b','a','k','e','r'],['l','e','v','e','r']] // ["baker"]
//Start = crosswordWords [['e','a','g','e','r'],['e','a','g','l','e'],['j','o','b'],['h','o','m','e']]//[]



/*9. Triples
You are given a list. Return a list of tuples containing 
two adjacent elements, and their sum.
Example: [1, 2, 3] -> [(1, 2, 3), (2, 3, 5)]
Explanation:
(1, 2, 3) -> 1 and 2 are adjacent numbers, and 3 is their sum
(2, 3, 5) -> 2 and 3 are adjacent numbers, and 5 is their sum
*/

adjacent_sum :: [Int] -> [(Int, Int, Int)]
adjacent_sum list = [(x,y,x+y)\\x<-list&y<-(tl list)]

//Start = adjacent_sum [1..3] // [(1,2,3),(2,3,5)]
//Start = adjacent_sum [1..5] // [(1,2,3),(2,3,5),(3,4,7),(4,5,9)]
//Start = adjacent_sum [8, 5, 3, 6] // [(8,5,13),(5,3,8),(3,6,9)]
//Start = adjacent_sum [81..84] // [(81,82,163),(82,83,165),(83,84,167)]
//Start = adjacent_sum [1..8] // [(1,2,3),(2,3,5),(3,4,7),(4,5,9),(5,6,11),(6,7,13),(7,8,15)]



/*10. Good number
A number is good if the digits (0-indexed) at even indices are even 
and the digits at odd indices are prime (2, 3, 5, or 7).
For example: 
2582 is good because the digits (2 and 8) at even positions are even 
and the digits (5 and 2) at odd positions are prime. 
3245 is not good because 3 is at an even index but is not even.
Write a function that takes list and outputs a list only with good numbers.
*/

isPrime :: Int -> Bool
isPrime n = length[div\\div<-[1..n]|n rem div==0]==2

isGoodNumber :: Int -> Bool
isGoodNumber n = length[el\\el<-(numToList n)&i<-[0..length (numToList n)]|if (isOdd i) (isPrime el) (isEven el)]==length(numToList n)

goodNumbers :: [Int] -> [Int]
goodNumbers list = [el\\el<-list|isGoodNumber el]

//Start = goodNumbers [2582, 3245]//[2582]
//Start = goodNumbers [2,4,6,8]//[2,4,6,8]
//Start = goodNumbers [2,3,5,7]//[2]
//Start = goodNumbers [636, 4289, 28012, 2702, 9742, 1273, 4242, 138264, 4387]//[636, 2702, 4242, 4387]
//Start = goodNumbers [6205, 100, 255, 98314, 4763, 2500, 472, 9743725] // [6205, 4763, 472] 



/*11. Indexing
Given a list of 2 element tuple of integer, transform each tuple 
into 2 tuples with its index.
Given: 	[(2,3),(4,4),(3,2),(4,2),(3,4),(6,7)]
index:	   0     1     2     3     4     5		
		(2,3) and its position is 0 => (2,0) and (3,0)
		(4,4) and its position is 1 => (4,1) and (4,1)
		(3,2) and its position is 2 => (3,2) and (2,2)		
Result: [(2,0),(3,0),(4,1),(4,1),(3,2),(2,2)]		
*/

splitTupleAux :: (Int,Int) Int -> [(Int, Int)]
splitTupleAux tuple i = [(fst tuple,i),(snd tuple,i)]

splitTuple :: [(Int,Int)] -> [(Int,Int)]
splitTuple list = flatten[splitTupleAux el i\\el<-list&i<-[0..length list]]

//Start = splitTuple [(2,3),(4,4),(3,2)] // [(2,0),(3,0),(4,1),(4,1),(3,2),(2,2)]
//Start = splitTuple [(7,4),(8,9)] // [(7,0),(4,0),(8,1),(9,1)]
//Start = splitTuple [(4,2)] // [(4,0),(2,0)]
//Start = splitTuple [] // []



/*12. Teams
Several people are standing in a row divided into two teams.
The first person goes into team 1, the second goes into team 2, 
the third goes into team 1, and so on.
Given a list of positive integers (the weights of the people), 
return a new tuple of two integers, where the first one is the 
total weight of team 1, and the second one is the total weight of team 2

[13, 27, 49]  ==>  (62, 27)
The first element 62 is the total weight of team 1, and the second 
element 27 is the total weight of team 2.

[50, 60, 70, 80]  ==>  (120, 140)
The first element 120 is the total weight of team 1, and the second 
element 140 is the total weight of team 2.
*/

rowWeights :: [Int] -> (Int,Int)
rowWeights list = (sum[el\\el<-list&i<-[0..length list]|isEven i],sum[el\\el<-list&i<-[0..length list]|isOdd i])

//Start = rowWeights [] // (0,0)
//Start = rowWeights [70,90] // (70,90)
//Start = rowWeights [13, 27, 49] // (62, 27)
//Start = rowWeights [50, 60, 70, 80] //  (120, 140)



/*13. Automorphic
An automorphic number, also known as a circular number, is a number 
whose square ends with the same digits as the number itself. 
When the square of a number is written, the original number 
is found at the end of the result.
Given a list of integers, return only the automorphic numbers of the list.

5 is an automorphic number because 5^2 = 25, and 5 is at the end of 25.
76 is an automorphic number because 76^2 = 5776, and 76 is at the end of 5776.
625 is an automorphic number because 625^2 = 390625, and 625 is at the end of 390625.
*/

automorphicNumber :: [Int] -> [Int]
automorphicNumber list = [el \\ el <- list | drop (length (numToList el)) (numToList (el^2)) == (numToList el) ]

//Start = automorphicNumber [5, 76, 625, 376, 9376] // [5,76,625,376,9376]
//Start = automorphicNumber [3, 12, 34, 100] // []
//Start = automorphicNumber  [25, 4, 12, 376, 625, 10] // [25,376, 625]
//Start = automorphicNumber  [] // []
//Start = automorphicNumber  [1] // [1]

