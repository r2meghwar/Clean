module HW8GR1
import StdEnv

// REMEMBER : COPYING OR GIVING A COPY MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW8GR1 ( e.g JohnSmithHW5GR1)
Also, don't forget to change filename in the first line of file  */

//Please write your neptun code here:

/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/



:: Tree a = Node a (Tree a) (Tree a) | Leaf
tree1 = Node 7 
						( Node 2 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf)) 
						( Node 20 (Node 12 Leaf Leaf) (Node 4 Leaf Leaf))
						
						
tree2 = Node 5 
						( Node 3 (Node 13 Leaf Leaf) (Node 11 Leaf Leaf)) 
						( Node 1 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf))



/*
    1. Write a function that takes a tree and a list of tuples of the form (a,b), 
    you need to find the node with value a and change its value to b times
    its level in the tree.

    eg:
    
    input: 
             7                    1st level
           /   \                
          2     20               2nd level
         / \    / \ 
       10  30  12  4        3rd level

        [(10,2),(30,3),(4,6),(20,5)]

       output 
                  7
                /   \
               2     10
              / \    / \
            6   9  12   18
    (10,2) => 10 is at level 3, so its value is changed to 2*3 = 6
    (30,3) => 30 is at level 3, so its value is changed to 3*3 = 9
    (4,6) => 4 is at level 3, so its value is changed to 6*3 = 18
    (20,5) => 20 is at level 2, so its value is changed to 5*2 = 10

*/


SwapLevelAux :: (Tree Int) Int Int Int -> (Tree Int)
SwapLevelAux Leaf a b level = Leaf
SwapLevelAux (Node x left right) a b level
| x == a = (Node (b*level) left right)
= (Node x (SwapLevelAux left a b (level+1)) (SwapLevelAux right a b (level+1)))
 
SwapLevel :: (Tree Int)  [(Int,Int)] -> (Tree Int)
SwapLevel tree [] = tree
SwapLevel tree [(a,b):xs] = SwapLevel (SwapLevelAux tree a b 1) xs


//Start = SwapLevel tree1 [(10,2),(30,3),(4,6),(20,5)]
//(Node 7 
//          (Node 2 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf)) 
//          (Node 10 (Node 12 Leaf Leaf) (Node 18 Leaf Leaf)))


//Start = SwapLevel tree2 [(13,7),(11,1),(1,5)] 
//(Node 5
//          (Node 3 (Node 21 Leaf Leaf) (Node 3 Leaf Leaf))
//          (Node 10 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf)))


// Task 2

:: Genre = Fiction | NonFiction | Science | History

:: Book = { bookName :: String, ratings :: [Int], author :: String, genre :: Genre }
// 'ratings' is a list of integers where each integer represents a rating from 1 to 5

:: Library = { libraryName :: String, numberOfBooks :: Int, books :: [Book] }

b1 :: Book
b1 = { bookName = "Book One", ratings = [5, 4, 5, 4, 5], author = "Author A", genre = Fiction }

b2 :: Book
b2 = { bookName = "Book Two", ratings = [3, 3, 3, 4, 2], author = "Author B", genre = NonFiction }

b3 :: Book
b3 = { bookName = "Book Three", ratings = [4, 4, 4, 4, 4], author = "Author C", genre = Science }

b4 :: Book
b4 = { bookName = "Book Four", ratings = [2, 2, 3, 1, 1], author = "Author D", genre = History }

b5 :: Book
b5 = { bookName = "Book Five", ratings = [5, 5, 5, 5, 5], author = "Author E", genre = Fiction }

b6 :: Book
b6 = { bookName = "Book Six", ratings = [1, 2, 1, 2, 2], author = "Author F", genre = History }

l1 :: Library
l1 = { libraryName = "Central Library", numberOfBooks = 3, books = [b1, b3, b5] }

l2 :: Library
l2 = { libraryName = "Town Library", numberOfBooks = 2, books = [b1, b2] }

l3 :: Library
l3 = { libraryName = "City Library", numberOfBooks = 3, books = [b1, b3, b4] }

l4 :: Library
l4 = { libraryName = "Regional Library", numberOfBooks = 3, books = [b1, b3, b5] }

l5 :: Library
l5 = { libraryName = "Community Library", numberOfBooks = 3, books = [b2, b3, b6] }


instance == Genre
where
  (==) History History = True
  (==) Science Science = True
  (==) NonFiction NonFiction = True
  (==) Fiction Fiction = True
  (==) _ _ = False

/*
2.

Write a function that calculates the average rating for a specific genre 
within a library and identifies which library has the highest average rating for that genre

*/


Aux4 :: [(Real,String)] -> String
Aux4 ls = hd [ snd x \\ x <- ls | maxList list == fst x]
where list = map (\x = fst x) ls

Aux3 :: [Real] String -> [(Real,String)]
Aux3 ls name = [ (x, name) \\ x <- ls]

Aux2 :: [Book] Genre -> [Real]
Aux2 books genre = [ toReal (sum book.ratings) / toReal (length (book.ratings)) \\ book <- books | book.genre == genre ]

Aux1 :: Library Genre String -> [(Real,String)]
Aux1 library genre name = Aux3 (flatten [ Aux2 (library.books) genre ]) name

maxRatingLibraryByGenre :: [Library] Genre -> String
maxRatingLibraryByGenre libraries genre = Aux4 (flatten [Aux1 library genre (library.libraryName) \\ library <- libraries ])


//Start = maxRatingLibraryByGenre [l1,l2,l3,l4,l5] History // City Library
//Start = maxRatingLibraryByGenre [l1,l2,l3,l4,l5] Fiction // Central Library
//Start = maxRatingLibraryByGenre [l1,l2,l3,l4,l5] NonFiction // Town Library


