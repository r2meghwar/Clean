module HW10GR1
import StdEnv

// REMEMBER : PLAGIARISM MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW10GR1 ( e.g JohnSmithHW10GR1)
Also, don't forget to change filename in the first line of file  */


/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/ 

::Movie = {
			title::String,
			casts::[Cast],
			year::Int,
			rating::Real,
			country::String
			}
::Gender = Male | Female
::Cast = {
			name::String,
			gender::Gender
			}

cast1 = { name="Jackie Chan", gender= Male}
cast2 = { name="Jet Li", gender= Male }
cast3 = { name="Millie Bobby Brown", gender=Female}
cast4 = { name="Chris Hemsworth", gender=Male }
cast5 = { name="Zendaya", gender=Female }
cast6 = { name="Emma Stone", gender=Female }
cast7 = { name="Emma Watson", gender = Female }
cast8 = { name="Sandra Bullock", gender = Female}
cast9 = { name="Chris Evans", gender= Male}
cast10 = { name="Tom Holland", gender=Male}
cast11 = { name="Tobey Maguire", gender=Male}
cast12 = { name="Bae Suzy", gender=Female}
cast13 = { name="Park Seo Joon", gender=Male}

movie1 = { title="MOVIE I.", casts=[cast1, cast6, cast10], year=2019, rating=8.5, country="USA"}
movie2 = { title="MOVIE II.", casts=[cast4, cast8, cast7], year=2020, rating=8.0, country="Spain"}
movie3 = { title="MOVIE III.", casts=[cast13, cast12, cast9], year=2019, rating=9.0, country="Korea"}
movie4 = { title="MOVIE IV.", casts=[cast3, cast11, cast8], year=2021, rating=6.5, country="India"}
movie5 = { title="MOVIE V.", casts=[cast2, cast4, cast10], year=2022, rating=7.4, country="Hungary"}
movie6 = { title="MOVIE VI.", casts=[cast3, cast5, cast8], year=2022, rating=7.4, country="Hungary"}


/*

1. Given a list of movies, return the name of the movie that has the lowest rating.

*/


movieMinRating :: [Movie] -> String
movieMinRating ls = fst (hd [ (el.title,el.rating) \\ el <- ls | minList RatingList == el.rating ])
where RatingList = [ el.rating \\ el <- ls]

//Start = movieMinRating [movie1,movie2,movie3,movie4,movie5, movie6] 	// "MOVIE IV."



:: Tree a = Node a (Tree a) (Tree a) | Leaf

bestTree  = Node 10(Node 6(Node 1 Leaf(Node 5(Node 2 Leaf(Node 4(Node 3 Leaf Leaf)Leaf))Leaf))Leaf)(Node 14(Node 11 Leaf(Node 13(Node 12 Leaf Leaf)Leaf))(Node 17(Node 15 Leaf(Node 16 Leaf Leaf))(Node 19(Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))
ourTree   = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
shortTree = Node 14(Node 11 Leaf(Node 13 Leaf Leaf))(Node 17(Node 15 Leaf Leaf)Leaf)
noTree = Leaf
unitTree = Node 1337 Leaf Leaf


/*
 2. Define a tree type and write a function that takes a tree as a parameter
 and returns a list of the numbers of the nodes whose children are both Leaf.
 an empty tree will return [] and a single element tree will return a list of one element
*/

leaves :: (Tree Int) -> [Int]
leaves Leaf = []
leaves (Node x Leaf Leaf) = [x]
leaves (Node _ l r) = leaves l ++ leaves r 

//Start = leaves bestTree //[3,12,16,18,20]
//Start = leaves ourTree //[1,8,11,19,24,28]
//Start = leaves unitTree //[1337]
//Start = leaves noTree //[]




