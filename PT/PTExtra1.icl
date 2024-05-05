module PTExtra1
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf
          
/*
	Given a tree of integer and check if each node has even left child and odd right child.
					 10
				   /    \
				  2      3
				 / \    / \
				4   7  6   5
			   / \ / \/ \ / \
			  8   LL LL LL   1
			  
	That tree given returns TRUE because each node has even left child and odd right child.
			  
	Hint: you could write 2 extra functions for checking if a node is Even, and if a node is odd.
	
*/

treeOne = Node 10 (Node 2 (Node 4 (Node 8 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 5 Leaf (Node 1 Leaf Leaf)))
treeTwo = Node 10 (Node 2 (Node 4 (Node 9 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 5 Leaf (Node 1 Leaf Leaf))) 
treeThree = Node 10 (Node 2 (Node 4 (Node 8 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 5 Leaf (Node 2 Leaf Leaf))) 


isEvenTree :: (Tree Int) -> Bool
isEvenTree Leaf = True
isEvenTree (Node x left right) = isEven x && isEvenTree left && isOddTree right

isOddTree :: (Tree Int) -> Bool
isOddTree Leaf = True
isOddTree (Node x left right) = isOdd x && isEvenTree left && isOddTree right

PT8 :: (Tree Int) -> Bool
PT8 Leaf = True
PT8 (Node x left right) = isOddTree right && isEvenTree left

//Start = PT8 treeOne		// True
//Start = PT8 treeTwo 	// False
//Start = PT8 treeThree 	// False


















