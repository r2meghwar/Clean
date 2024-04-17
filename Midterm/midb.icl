module midb
import StdEnv


/* WRITE <Raja Roshan Meghwar> AND <BQLAGT> HERE
by this YOU DECLARE this FILE is 
YOUR OWN SOLUTION for functional 
programming midterm 2024 April 17.*/


/*1. McCarthy function
McCarthy is a renowned computer science theorist who defined a 
recursive function called f91. It takes a positive integer N as input 
and returns a positive integer following these rules:
 
If N is less than or equal to 100, then f91(N) equals f91(f91(N + 11)).
If N is greater than or equal to 101, then f91(N) equals N - 10.
Write a program to compute McCarthy's function f91 value.
If the input number is less than 1, abort with "Stop".
*/

f91 :: Int -> Int
f91 n
| n >= 1 && n <= 100 = f91 (f91 (n+11))
| n >= 101 = n - 10
= abort "Stop"

//Start= f91 500 //490
//Start= f91 91 //91
//Start= f91 0 //Stop
//Start= f91 -100//Stop


/*2. Count steps
Write a recursive function to count the minimum number of steps 
required to reduce a positive integer to 1.
Allowed steps are: subtract 1, divide by 2 (if divisible), 
or divide by 3 (if divisible).
*/

countStepsToOneAux :: Int -> Int
countStepsToOneAux n
| n <= 1 = 0
| n rem 2 == 0 = 1 + countStepsToOneAux (n/2)
| n rem 3 == 0 = 1 + countStepsToOneAux (n/3)
= 1 + countStepsToOneAux (n-1)

countStepsToOne :: Int -> Int 
countStepsToOne n = countStepsToOneAux n

//Start = countStepsToOne 1 //  0 
//Start = countStepsToOne 2 //  1 
//Start = countStepsToOne 6 //  2
//Start = countStepsToOne 10 //  4
//Start = countStepsToOne 27 //  3
//Start = countStepsToOne 30 //  5


/*3. Position
Given a list, and a number, find the number in the list.
If it is present, return its index. If not, return -1.
(you can assume that the number is present only once)
*/

position :: [Int] Int -> Int
position list n
| isMember n list = toNum [i \\ x <- list & i <- [0..length list] | x == n]
= -1

//Start = position [1, 2, 3, 4, 5] 3 // 2
//Start = position [8, 5, 3, 6] 3 // 2
//Start = position [8, 5, 3, 6] 2 // -1
//Start = position [5, 8, 32, 7, 2, 6, 9, 12, 52, 3] 5 // 0
//Start = position [5, 8, 32, 7, 2, 6, 9, 12, 52, 3] 12 // 7
//Start = position [5, 8, 32, 7, 2, 6, 9, 12, 52, 3] 3 // 9


/*4. Split sums
Given an integer, split it into two integers and then sum them.
Return a list of sums.

Splitsum 1234 = [235,   46,   127]
				1|234  12|34  123|4  
				1+234  12+34  123+4  
*/

toNum :: [Int] -> Int
toNum [] = 0
toNum [x:xs] = x * 10 ^ length xs + toNum xs 
 
toList :: Int -> [Int]
toList 0 = []
toList n = toList (n/10) ++ [n rem 10] 
 
splitsum :: Int -> [Int]
splitsum n = [toNum (take i (toList n)) + toNum (drop i (toList n))  \\ x <- toList n & i <- [1..length (toList n)-1]]
 
//Start = splitsum 888 //[96,96]
//Start = splitsum 1234 //[235,46,127]
//Start = splitsum 0 // []
//Start = splitsum 123321 //[23322,3333,444,1254,12333]


/*5. Apply
Write a function that takes a function and two lists 
and applies the function to all elements
of the lists and returns the new list with output.
*/

//functions for test
summ x n = x+n
prodd x n = x*n
joinn x y = x +++ y

apply :: (a a -> a) [a] [a] -> [a]
apply func ls1 ls2 = [func x y \\ x <- ls1 & y <- ls2]

//Start = apply summ [1,2,3,4,5] [4,5,6]  // [5,7,9]
//Start = apply prodd [1,2,3] [4,5,6,7] // [4,10,18]
//Start = apply joinn ["Hello","Hi","Ciao","Szia"] ["World","John"] // ["HelloWorld","HiJohn"]


/*6. Pairs
Given two lists of integers, list1 and list2, and a target number, 
find all unique pairs where one element is taken from list1 and 
the other from list2, such that their sum equals the target number. 
The pairs should be returned as a list of tuples, with each tuple 
consisting of one integer from list1 and one integer from list2

Given [1,2,3] and [4,5,6], and target number 7, pairs that add to 7 
are (1,6), (2,5), (3,4). 
Return them as a list so final output is [[(1,6),(2,5),(3,4)]
*/

findPairSums :: [Int] [Int] Int -> [(Int,Int)]
findPairSums lsA lsB n = [(x,y)\\ x <- lsA , y <- lsB | x + y == n] 

//Start = findPairSums [1,2,3] [4,5,6] 7 // [(1,6),(2,5),(3,4)]
//Start = findPairSums [6, -2, 3] [5, 2, 7] 5 // [(-2, 7),(3, 2)]
//Start = findPairSums [-4,5,6] [4,-6,-5] 0 // [(-4,4),(5,-5),(6,-6)]


/*7. Update
Each tuple in a list indicates a key, value pair (key, value). 
Given a key and a value, if the new key already exists in the list, 
the value should be replaced, otherwise a new pair is added to the list. 

'b' 6 [('a', 5), ('b', 18), ('c', 7)] --> the value of b must be updated to 6 
== [('a', 5), ('b', 6), ('c', 7)]
*/

update :: Char Int [(Char, Int)] -> [(Char, Int)]
update char n list 
| isEmpty list = [(char,n)]
= [if (char <> fst x && n <> snd x) (x) (char,n) \\ x <- list]
	 
//Start = update 'b' 6 [('a', 5), ('b', 18), ('c', 7)] // [('a',5),('b',6),('c',7)]
//Start = update 'b' 6 [] // [('b',6)]


/*8. Append 
Write a function that takes a list of characters and creates 
a string by joining alternate characters together and 
then appending the two strings, assume lists to have 
even number of characters.

['a','b','c','d'] becomes "ac" + "bd" -> "acbd"
['d','o','m','i','n','o'] becomes "dmn" + "oio" -> "dmnoio"
*/

fa :: [Char] -> String
fa chars = toString [chars!!i \\i <- [0..length chars-1] | isEven i] +++ toString [chars!!i \\i <- [0..length chars-1] | isOdd i]

//Start = fa ['a','b','c','d'] // "acbd"
//Start = fa ['d','o','m','i','n','o'] // "dmnoio"


/*9. Game
In a football game, players typically play for 90 minutes unless they 
are substituted out. We are given a list of tuples, where each tuple 
represents a player. Each player tuple consists of their name (a string), 
a Boolean value indicating whether the player was subbed off and the minute 
at which they were subbed off (an integer).

If the Boolean value is False, it means the player was not substituted off and
his playtime was 90 minutes, despite what is written at the third field of the tuple.
Given a list of tuples, calculate the total playtime for all the players.

[("Player1", False, 129), ("Player2", True, 60), ("Player3", False, -99), ("Player4", True, 75) ]
Returns : 90 + 60 + 90 + 75 = 315
*/

playTime :: [(String, Bool, Int)] -> Int
playTime ls = sum [90 \\ x <- ls | snd3 x == False ] + sum [thd3 x \\ x <- ls | snd3 x == True ]

//Start = playTime [] // 0
//Start = playTime [("a", False, -1),("b", False, 0),("c", False, 0),("d", False, 12)] // 360
//Start = playTime [("a", True, 85),("b", True, 5),("c", True, 23),("d", True, 12)] // 125
//Start = playTime [("Player1", False, 129), ("Player2", True, 60), ("Player3", False, -99), ("Player4", True, 75) ] // 315


/*10. Scramble
Define a function scramble that takes a list and puts all the 
elements at even positions before the elements at odd position. 
Positions are numbered starting at 0, so first element is at an even position. 

scramble [0..10] == [0,2,4,6,8,10,1,3,5,7,9] 
*/

scramble :: [a] -> [a]
scramble ls = [x \\ x <- ls & i <- [0..length ls - 1] | isEven i] ++ [x \\ x <- ls & i <- [0..length ls - 1] | isOdd i]

//Start = scramble [0,1,2] // [0,2,1]
//Start = scramble [0..10] // [0,2,4,6,8,10,1,3,5,7,9]
//Start = scramble ['F','u','n','c','t','i','o','n','a','l',' ','P','r','o','g','r','a','m','m','i','n','g'] //['F','n','t','o','a',' ','r','g','a','m','n','u','c','i','n','l','P','o','r','m','i','g']


/*11. Differences average
Given a list of integers, which is sorted in ascending order, 
implement a function which computes the average differences between 
two adjacent integers.

Input: [1, 4, 9]
Output: 4
The difference from 1 to 4 is 3, and from 4 to 9 is 5, thus the differences' average is (3 + 5)/2=4

Input: [3,7,12,13]
Output: 3.666
Explanation ((7-3)+(12-7)+(13-12))/3 = 3.6667
*/

averageDifference :: [Int] -> Real
averageDifference list = toReal (sum [ (y-x) \\ x <- list & y <- (drop 1 list)]) / toReal (length list - 1) 

//Start = averageDifference [14, 16, 20, 34, 59, 71] 	// Approx: 0.734
//Start = averageDifference [18, 22, 24, 4, 76, 82] 	// Approx: 1.743
//Start = averageDifference [32, 45, 47, 54, 64, 72] 	// Approx: 0.854
//Start = averageDifference [44, 56, 64, 67, 69, 84] 	// Approx: 0.882


/*12. Triple tuples
Given a 2-dimensional list of integer, generate the list of 3 element tuple
(first, mid, last) for each sublist.
If the list has even number of elements, use -1 as a mid element.
If it is an empty list, use -1 for first, mid and last.
	
[9,8,5,0] => (9,-1,0)
[9,8,5,3,4] => (9,5,4)
[1,2,3] => (1,2,3)
[2,3] => (2,-1,3)
[5] => (5,5,5)
[] => (-1,-1,-1)
				 
Given [[9,8,5,0],[9,8,5,3,4],[1,2,3]] => [(9,-1,0), (9,5,4), (1,2,3)]
*/

midElement :: [Int] -> Int
midElement list 
| isEven (length list) = -1
= minList (take 1 (drop a list))
where a = length list/2

startMidEnd :: [[Int]] -> [(Int,Int,Int)]
startMidEnd [] = []
startMidEnd list 
= [if (isEmpty x) (-1,-1,-1) (hd x, midElement x, last x) \\ x <- list]

//Start = startMidEnd [[9,8,5,0],[9,8,5,3,4],[1,2,3]] // [(9,-1,0),(9,5,4),(1,2,3)]
//Start = startMidEnd [[2,3],[5],[]] // [(2,-1,3),(5,5,5),(-1,-1,-1)]
//Start = startMidEnd [] // []
//Start = startMidEnd [[-1,-1,-1],[]] // [(-1,-1,-1),(-1,-1,-1)]


/*13. Frequent names
You are given a list of names of students of the same faculty. 
Find and output name that occurs most frequently.
Output all names that have maximum number of occurrences.
*/

Occurrence :: String [String] -> Int
Occurrence str list = length [ (str,x) \\ x <- list | str == x]

OccAux :: [Int] [String] -> [String]
OccAux list str = [x \\ x <- str & y <- list | y == maxList list ]

mostFreqName :: [String] -> [String]
mostFreqName ls = removeDup (OccAux ([Occurrence x ls \\ x <- ls]) ls)

//Start = mostFreqName ["Emma", "Noah", "Olivia", "Emma", "Ava", "Liam", "Sophia", "Isabella", "Mia", "Amelia", "Harper", "Ethan", "Emma", "Charlotte", "Olivia", "Noah", "Ava", "Mia", "Liam", "Ethan", "Emma", "Noah", "Liam", "Ava", "Sophia", "Noah"]
// --> ["Emma","Noah"] - all of them are occuring 4 times in the list
//Start = mostFreqName ["James", "Mary", "John", "Patricia", "Robert", "Jennifer", "Michael", "Linda", "William", "Elizabeth", "David", "Barbara", "Richard", "Susan", "Joseph", "Jessica", "Thomas", "Sarah", "Charles", "Karen", "Christopher", "Nancy", "Daniel", "Lisa", "Matthew", "Betty", "Anthony", "Margaret", "Mark", "Sandra", "Elizabeth", "Michael", "James", "David", "Charles", "Joseph", "Thomas"]
// --> ["James","Michael","Elizabeth","David","Joseph","Thomas","Charles"] - all of them are occuring 2 times in the list







