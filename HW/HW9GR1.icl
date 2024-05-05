module HW9GR1

import StdEnv

// THIS IS OPTIONAL HOMEWORK.

// REMEMBER : COPYING OR GIVING A COPY MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW9GR1 ( e.g JohnSmithHW9GR1)
Also, don't forget to change filename in the first line of file  */

//Please write your neptun code here: 

/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/


// TASK 1


:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf


tree1 = Node 10 (Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 9 Leaf Leaf)) (Node 7 Leaf Leaf)) (Node 15 (Node 12 Leaf Leaf) (Node 18 Leaf Leaf))
tree2 = Node 15 (Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 12 Leaf Leaf)) (Node 18 (Node 16 Leaf (Node 14 Leaf Leaf)) (Node 20 Leaf Leaf))
tree3 = Node 12 (Node 8 (Node 4 Leaf Leaf) (Node 7 (Node 6 Leaf Leaf) Leaf)) (Node 10 Leaf (Node 15 Leaf (Node 14 Leaf Leaf)))

/*

Given a binary tree, where each node has an integer value, 
calculate the sum of all the node values that are greater than 5 at a given depth d.

example:
             10                level 1
            / \
           5   15            level 2
          / \  / \
         3   7 12 18           level 3
        / \
       1   9					level 4
       
       
       d = 3;
       output : 40
       explanation : node values at level 3 are 3, 7, 12 and 18. 3 is smaller than 5. 
       sum of others are 37
       
*/

sumDepthHelper :: (Tree Int) Int Int -> Int
sumDepthHelper Leaf _ _ = 0
sumDepthHelper (Node x left right) d currentLevel
| currentLevel == d && x > 5 = x + sumDepthHelper left d (currentLevel+1) + sumDepthHelper right d (currentLevel+1)
= sumDepthHelper left d (currentLevel+1) + sumDepthHelper right d (currentLevel+1)       
      
sumDepth :: (Tree Int) Int -> Int
sumDepth tree d = sumDepthHelper tree d 1

//Start = sumDepth tree1 3 // 37
//Start = sumDepth tree2 4 // 21
//Start = sumDepth tree3 3 // 22
//Start = sumDepth tree2 2 // 28



// TASK 2


::MenuItem = { name::String, price::Int }
				
::Restaurant = { restname::String, menu::[MenuItem], rating::Real }
				
item1 = { name="abc", price=2500 }
item2 = { name="def", price=1800 }
item3 = { name="def", price=2000 }
item4 = { name="ghi", price=1500 }
item5 = { name="jkl", price=2500 }
item6 = { name="mno", price=1800 }
item7 = { name="pqr", price=1500 }
item8 = { name="stu", price=3000 }
item9 = { name="vwx", price=3200 }
item10 = { name="yz", price=3150 }

rest1 = { restname="Restaurant I.", menu=[item5, item8, item9], rating=10.0 }
rest2 = { restname="Restaurant II.", menu=[item2, item4, item6], rating=8.5 }
rest3 = { restname="Restaurant III.", menu=[item7, item10, item1, item3], rating=7.2 }
rest4 = { restname="Restaurant IV.", menu=[item1, item2, item3], rating=9.5 }

restaurants = [ rest1, rest2, rest3, rest4 ]

/*
	Find the name of the restaurant that 
	is the most expensive (where the average cost of the menu item is maximum)
	Find the average rating of a restaurant.
	
	Return the both value in tuple.
*/

avgRating :: [Restaurant] -> Real
avgRating ls = toReal (sum [x.rating \\ x <- ls]) / toReal (length ls)

avgPrice :: [MenuItem] -> Real
avgPrice ls = toReal (sum [x.price \\ x <- ls]) / toReal (length ls)

expensiveRestaurant :: [Restaurant] -> (String,Real)
expensiveRestaurant [] = ("",0.0)
expensiveRestaurant ls = hd [(x.restname, avgRating ls) \\ x <- ls | maxList list == avgPrice x.menu]
where list = [avgPrice x.menu \\ x <- ls]

//Start = expensiveRestaurant restaurants	// ("Restaurant I.",8.8)
//Start = expensiveRestaurant []		// ("",0)









