module endretake
import StdEnv


/* Write <NAME> and <NEPTUN CODE> here.
by this YOU DECLARE this file is 
YOUR OWN SOLUTION for functional 
programming endterm retake 2024 May 30.
If name and neptun missing, 
we will not check the file!!!*/


// Each exercise is of 10 points.

/*1. Arrays
Given the arrays of list of Int and a callback predicate function (Int -> Bool), 
For each array, remove the elements that the predicate function is returning false.
For example:
[{1,2,4,5,6,8},{9,10,3,2,1},{6,5,8,3}] and isEven
For each array, remove the elements that isEven function is returning false.
Result:	[{2,4,6,8},{10,2},{6,8}]	
*/

filterArrays :: [{Int}] (Int -> Bool) -> [{Int}]
filterArrays list func = [{x\\x<-:el|func x} \\ el <- list]

//Start = filterArrays [{1,2,4,5,6,8},{9,10,3,2,1},{6,5,8,3}] isEven // [{2,4,6,8},{10,2},{6,8}]
//Start = filterArrays [{}, {1,2,3,4}, {-1,-2,5}] ((<)5) // [{},{},{}]
//Start = filterArrays [{0,1,2,3},{0,0,1,2,3}] ((==)0) // [{0},{0,0}]
//Start = filterArrays [] isOdd // []



/*2. Sums
Write a function that takes an array of integers as input and modifies the array by 
removing all unique elements and then finding average of the remaining elements. 
A unique element is one that appears only once in the array. 
If an element is repeated, it should be added to the sum for each occurrence and 
average of that sum should be found.
Eg. - input: {1,2,2,3,3,5}, output: 2.5
explanation: 1 and 5 is removed from array because they occur only once.
average of other elements in the list is 2.5 ((2+2+3+3)/4)
*/

Occurrence :: Int {Int} -> Int
Occurrence n ls = length [el \\ el <-: ls | n == el]

sumRepeatingElements :: {Int} -> Real
sumRepeatingElements ls = toReal (sum list) / toReal (length list) 
where list = [el \\ el <-: ls | Occurrence el ls > 1]

//Start = sumRepeatingElements {1,2,2,3,3,5} // 2.5
//Start = sumRepeatingElements {7,7,7,8,6,6,5} // 6.6



/*3. Vowels count
Write a function that accepts a string as input and returns an array of tuples. 
Each tuple in the array should contain two elements: a vowel from the English 
language ('a', 'e', 'i', 'o', 'u') and the number of times that vowel appears 
in the input string. No duplicates should be present. 
*/

isVowel :: Char -> Bool
isVowel char = isMember char ['a','e','i','o','u']

Occ :: Char String -> Int
Occ ch str = length [el \\ el <-: str | el == ch]

vowels :: String -> {(Char, Int)}
vowels str = {(el, Occ el str) \\ el <- list | isVowel el}
where list = removeDup [el \\ el <-: str]

//Start = vowels "" // {}
//Start = vowels "mnsncdlkw" // {}
//Start = vowels "bbaaeeiioouubb" // {('a',2),('e',2),('i',2),('o',2),('u',2)}
//Start = vowels "hello dear friend this is a cool message" // {('e',5),('o',3),('a',3),('i',3)}



/*4. Phones 
Given an array of phone numbers as a String, group them into subarray by the 
country code. Assume the country code is the first two digits of the phone number. 
*/

take2 :: String -> String
take2 str = {x \\ x <- take 2 [y \\ y <-: str]}

group :: {String} -> {{String}}
group ls = {{el \\ el <-: ls | take2 el == a} \\ a <- countryCode}
where countryCode = removeDup [take2 el \\ el <-: ls]

//Start = group {"36204847320", "364859347", "96606994", "30211234567", "30291234567"}
// {{"36204847320", "364859347"}, {"96606994"}, {"30211234567", "30291234567"}}
//Start = group {"364859347"} // {{"364859347"}}
//Start = group {} // {}
//Start = group {"39204847320", "364859347", "96606994", "98211234567", "30291234567"}
//	{{"39204847320"},{"364859347"},{"96606994"},{"98211234567"},{"30291234567"}}



/*5. Modify
Write a function which takes an array of integers as the input.
The function keeps all elements in the array that occur more than twice, 
adds 5 to each of these elements, and returns a new array with the modified values.
*/

modify :: {Int} -> {Int}
modify list = {el + 5 \\ el <-: list | Occurrence el list > 2}

//Start = modify {1,2,3,3,4,4,4} // {9,9,9}
//Start = modify {1,2,2,3,3} // {}
//Start = modify {-1,-1,-1,-2,-2,-2,-5,-5,-5,-5} // {4,4,4,3,3,3,0,0,0,0}



/*6. Middle
You are going to be given a word. Your job is to return the middle character of the word.
If the word's length is odd, return the middle character. If the word's length is even,
return the middle 2 characters.
*/

middle :: String -> String
middle str
| isOdd (size str) = {x \\ x <- take 1 (drop (size str / 2) list)}
= {x \\ x <- take 1 (drop ((size str / 2)-1) list)} +++ {x \\ x <- take 1 (drop (size str / 2) list)}
where list = [el \\ el <-: str] 

//Start = middle "test" // "es"
//Start = middle "testing" // "t"
//Start = middle "middle" // "dd"
//Start = middle "A" // "A"


/*----------------------*/
:: Hospital = {hospitalName::String, patients::[Patient], doctors::[Doctor]}
:: Doctor = {name::String, specialty::String}
:: Patient = {patientName::String, age::Int, heartRates::{Int}, mostVisitedDoctor::Doctor}

GeneralHospital::Hospital
GeneralHospital={ hospitalName="General Hospital", patients=[Alice, Bob, Charlie], doctors=[DrSmith, DrJones]}

CityClinic::Hospital
CityClinic={ hospitalName="City Clinic", patients=[Diana, Eva], doctors=[DrBrown, DrWhite]}

HealthBridgeClinic::Hospital
HealthBridgeClinic={hospitalName="HealthBridge Clinic", patients=[Zoe, Liam, Mia, Noah], doctors=[DrGrey, DrBlack]}


DrSmith::Doctor
DrSmith={name="Dr. Smith", specialty="Cardiology"}

DrJones::Doctor
DrJones={name="Dr. Jones", specialty="Neurology"}

DrBrown::Doctor
DrBrown={name="Dr. Brown", specialty="Dermatology"}

DrWhite::Doctor
DrWhite={name="Dr. White", specialty="Orthopedics"}

DrGrey::Doctor
DrGrey={name="Dr. Grey", specialty="Pediatrics"}

DrBlack::Doctor
DrBlack={name="Dr. Black", specialty="General Practice"}


Alice::Patient
Alice={patientName="Alice", age=30, heartRates={72, 74, 73}, mostVisitedDoctor=DrSmith}

Bob::Patient
Bob={patientName="Bob", age=25, heartRates={68, 70, 69}, mostVisitedDoctor=DrJones}

Charlie::Patient
Charlie={patientName="Charlie", age=40, heartRates={66, 68, 67}, mostVisitedDoctor=DrSmith}

Diana::Patient
Diana={patientName="Diana", age=35, heartRates={75, 77, 76}, mostVisitedDoctor=DrWhite}

Eva::Patient
Eva={patientName="Eva", age=29, heartRates={71, 70, 72}, mostVisitedDoctor=DrBrown}

Zoe::Patient
Zoe={patientName="Zoe", age=22, heartRates={65, 67, 66}, mostVisitedDoctor=DrGrey}

Liam::Patient
Liam={patientName="Liam", age=24, heartRates={68, 69, 70}, mostVisitedDoctor=DrBlack}

Mia::Patient
Mia={patientName="Mia", age=28, heartRates={74, 72, 73}, mostVisitedDoctor=DrBlack}

Noah::Patient
Noah={patientName="Noah", age=32, heartRates={71, 69, 70}, mostVisitedDoctor=DrGrey}


/*7. Hospitals
Define < operator for Hospital type. One hospital is considered "smaller" than 
another hospital if number of patients multiplied by number of doctors in one hospital is 
less than number of patients multiplied by number of doctors in another hospital.

Example : CityClinic < GeneralHospital returns True because 
CityClinic has 2 patients and 2 doctors (2*2), 
meanwhile GeneralHospital has 3 patients and 2 doctors (3*2 =6).
*/

instance < Hospital
where
	(<) a b = (length a.patients) * (length a.doctors) < (length b.patients) * (length b.doctors)
	
//Start = CityClinic < GeneralHospital // True
//Start = HealthBridgeClinic < GeneralHospital // False



/*8. SortPatients
Given an array of patients, write a function which sorts the array:
using their age first. If two patients have the same age, the patients 
are sorted using their name in alphabetical order.
Return the array of names.
*/

sortPatients :: {Patient} -> {String}
sortPatients list = {el.patientName \\ a <- ageList, el <-: list | a == el.age }
where ageList = sort [el.age \\ el <-: list]

//Start = sortPatients {Bob, Charlie, Diana, Alice} // {"Bob","Alice","Diana","Charlie"}
//Start = sortPatients {Eva, Diana, Charlie, Bob, Alice} // {"Bob","Eva","Alice","Diana","Charlie"}
//Start = sortPatients {Bob, Eva, Diana, Charlie, Alice, Zoe} // {"Zoe","Bob","Eva","Alice","Diana","Charlie"}
//Start = sortPatients {Eva, Liam, Alice, Charlie, Bob, Diana, Zoe} // {"Zoe","Liam","Bob","Eva","Alice","Diana","Charlie"}
//Start = sortPatients {Mia, Eva, Alice, Diana, Liam, Zoe, Charlie, Bob} // {"Zoe","Liam","Bob","Mia","Eva","Alice","Diana","Charlie"}
//Start = sortPatients {Diana, Liam, Alice, Bob, Eva, Mia, Charlie, Noah} // {"Liam","Bob","Mia","Eva","Alice","Noah","Diana","Charlie"}
//Start = sortPatients {Mia, Liam, Alice, Noah, Eva, Charlie} // {"Liam","Mia","Eva","Alice","Noah","Charlie"}



/*----------------------*/

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf

/*9. Transform
Implement a function that takes a binary tree as input and 
transforms the tree based on the following rules:
1. Invert the tree by swapping the left and right children of every node.
2. Add the depth of the node to the value of each node in the tree.

example:
             10                depth 0
            /  \
           5    15             depth 1
          / \   / \
         3   7 12 18           depth 2
        / \
       1   9				   depth 3
       
   transformedTree:
   example:
             10             depth 0 
            /  \
           16   6           depth 1
          / \  / \
         20 14 9  5         depth 2
        	   	 / \
                12  4       depth3
*/

tree11 = Node 10 (Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 9 Leaf Leaf)) (Node 7 Leaf Leaf)) (Node 15 (Node 12 Leaf Leaf) (Node 18 Leaf Leaf))
tree22 = Node 15 (Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 12 Leaf Leaf)) (Node 18 (Node 16 Leaf (Node 14 Leaf Leaf)) (Node 20 Leaf Leaf))
tree33 = Node 12 (Node 8 (Node 4 Leaf Leaf) (Node 7 (Node 6 Leaf Leaf) Leaf)) (Node 10 Leaf (Node 15 Leaf (Node 14 Leaf Leaf)))

depth :: (Tree Int) -> Int
depth Leaf = 0
depth (Node x l r) = 1 + max (depth l) (depth r)

transform :: (Tree Int) Int -> (Tree Int)
transform Leaf _ = Leaf
transform (Node x l r) depth = Node (x + depth) (transform r (depth + 1)) (transform l (depth + 1))

transformTree :: (Tree Int) -> (Tree Int)
transformTree tree = transform tree 0

//Start = transformTree tree11 
//(Node 10 (Node 16 (Node 20 Leaf Leaf) (Node 14 Leaf Leaf)) (Node 6 (Node 9 Leaf Leaf) (Node 5 (Node 12 Leaf Leaf) (Node 4 Leaf Leaf))))
//Start = transformTree tree22
//(Node 15 (Node 19 (Node 22 Leaf Leaf) (Node 18 (Node 17 Leaf Leaf) Leaf)) (Node 11 (Node 14 Leaf Leaf) (Node 7 (Node 10 Leaf Leaf) (Node 6 Leaf Leaf))))
//Start = transformTree tree33
//(Node 12 (Node 11 (Node 17 (Node 17 Leaf Leaf) Leaf) Leaf) (Node 9 (Node 9 Leaf (Node 9 Leaf Leaf)) (Node 6 Leaf Leaf)))



/*10. Deconstruct
Given a tree containing String values, deconstruct the tree and 
return a string list such that the strings have the following structure:
every path should be built if the last node of paths has 2 leaves as children.

	   "house"
	  /		  \
   "bedroom"  "kitchen"
   /	\		/	\
"bed"	Leaf "table"  "spoon"  
/   \         /  \     /  \
Leaf Leaf  Leaf Leaf Leaf Leaf 

Result: ["house.bedroom.bed", "house.kitchen.table", "house.kitchen.spoon"] */


deconstructTree :: (Tree String) -> [String]
deconstructTree Leaf = []
deconstructTree (Node x Leaf Leaf) = [x]
deconstructTree (Node x l r) = [x +++ "." +++ y \\ y <- deconstructTree l] ++ [x +++ "." +++ y \\ y <- deconstructTree r]

//Start = deconstructTree (Node "house" (Node "bedroom" (Node "bed" Leaf Leaf) Leaf) (Node "kitchen" (Node "table" Leaf Leaf) (Node "spoon" Leaf Leaf)))
//["house.bedroom.bed","house.kitchen.table","house.kitchen.spoon"]
//Start = deconstructTree (Node "Exception" (Node "RuntimeException" Leaf Leaf) (Node "IOException" (Node "FileNotFound" Leaf Leaf) (Node "EOFException" Leaf Leaf)))
//["Exception.RuntimeException","Exception.IOException.FileNotFound","Exception.IOException.EOFException"]
//Start = deconstructTree (Node "String" (Node "slice" Leaf Leaf) Leaf) // ["String.slice"]



/*11. Reverse
Write a function oddreversetree, that takes a Int tree as input and returns a modified tree according to the following rule: 
For every node in the input tree, if the node's value is an odd number, reverse its left subtree and right subtree; 
otherwise, keep the original order.
*/

Tree1 :: Tree Int
Tree1 = Node 7 (Node 3 Leaf Leaf) (Node 9 Leaf Leaf)
Tree2 :: Tree Int
Tree2 = Node 0 (Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))  (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)) 
Tree3 :: Tree Int
Tree3 = Node 0 (Node 1 (Node 3 Leaf (Node 8 Leaf Leaf)) Leaf)  (Node 2 Leaf Leaf)

oddreversetree :: (Tree Int) -> (Tree Int)
oddreversetree Leaf = Leaf
oddreversetree (Node x l r)
| isOdd x = Node x (oddreversetree r) (oddreversetree l)
= Node x (oddreversetree l) (oddreversetree r) 

//Start=oddreversetree Tree1//(Node 7 (Node 9 Leaf Leaf) (Node 3 Leaf Leaf))
//Start=oddreversetree Tree2//(Node 0 (Node 1 (Node 4 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)))
//Start=oddreversetree Tree3//(Node 0 (Node 1 Leaf (Node 3 (Node 8 Leaf Leaf) Leaf)) (Node 2 Leaf Leaf))


