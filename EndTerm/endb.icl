module endb
import StdEnv


/* Write <Name> and <Neptune Id> here.
by this YOU DECLARE this file is 
YOUR OWN SOLUTION for functional 
programming endterm 2024 May 15.
If name and neptun missing, 
we will not check the file!!!*/


// Each exercise is of 10 points.

// Record type related

:: Hospital = {hospitalName::String, patients::[Patient], doctors::[Doctor]}
:: Doctor = {name::String, specialty::String}
:: Patient = {patientName::String, age::Int, heartRates::{Int}, mostVisitedDoctor::Doctor}

GeneralHospital::Hospital
GeneralHospital={hospitalName="General Hospital", patients=[Alice, Bob, Charlie], doctors=[DrSmith, DrJones]}

CityClinic::Hospital
CityClinic={hospitalName="City Clinic", patients=[Diana, Eva], doctors=[DrBrown, DrWhite]}

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



/*1. Avg patient
Given a hospital, determine the average age of the patients in that hospital.
*/


aux1:: [Patient] -> Real
aux1 list = toReal (sum [ el.age \\ el <- list]) / toReal (length list)

avg_age :: Hospital -> Real
avg_age hospital = aux1 (hospital.patients)

//Start = avg_age GeneralHospital // 31.6666666666667
//Start = avg_age CityClinic // 32
//Start = avg_age HealthBridgeClinic // 26.5



/*2. Instance patient
Create an instance of the < operator for Patient type. 
Supposing p1 and p2 are of Patient type, p1 < p2 holds 
if p1 has lower average heart rate than p2.
*/

instance < Patient
where
	(<) a b = (sum list1) / (length list1) < (sum list2) / (length list2)
	where list arr = [el \\ el <-: arr ] 
		  list1 = list (a.heartRates)
		  list2 = list (b.heartRates)



//Start = Alice < Bob // False
//Start = Charlie < Bob // True
//Start = Bob < Liam // False



/*3. Find patient
Given a hospital and a doctor name (String).
Find all the patients' name of that hospital (put them in a list), 
that are associated with the given doctor's name.
Filter the patient list of that hospital with the given doctor's name,
the mostVisitedDoctor in the patient represents the doctor associated 
with that patient.
*/
	
Aux2 :: [Patient] String -> [String]
Aux2 list str = [ el.patientName \\ el <- list | el.mostVisitedDoctor.name == str ] 

findPatient :: Hospital String -> [String]
findPatient hospital str = Aux2 (hospital.patients) str

//Start = findPatient HealthBridgeClinic "Dr. Black" // ["Liam","Mia"]
//Start = findPatient CityClinic "Dr. White" // ["Diana"]
//Start = findPatient GeneralHospital "Dr. Smith" // ["Alice","Charlie"]



/*4. Nr. of patients
Given an array of hospitals, for each doctor in each hospital, count how many 
patient he has, such that the patient's mostVisitedDoctor is himself. 
Example: 
General hospital -> [(DrSmith, 2), (DrJones, 1)] 
DrSmith is the most visited doctor of Alice and Charlie,
DrJones is the most visited doctor of Bob. */


Aux7 :: String [Patient] -> Int
Aux7 str list = length [1 \\ el <- list | el.mostVisitedDoctor.name == str]

Aux6 :: [Doctor] [Patient] -> [(Doctor, Int)]
Aux6 list1 list2 = [(el1, Aux7 el1.name list2 ) \\ el1 <- list1 ]

hospitalPatientCount :: {Hospital} -> [[(Doctor, Int)]]
hospitalPatientCount list = [ Aux6 (el.doctors) (el.patients) \\ el <-: list ]


//Start = hospitalPatientCount {GeneralHospital, CityClinic} // [[((Doctor "Dr. Smith" "Cardiology"),2),((Doctor "Dr. Jones" "Neurology"),1)],[((Doctor "Dr. Brown" "Dermatology"),1),((Doctor "Dr. White" "Orthopedics"),1)]]
//Start = hospitalPatientCount {HealthBridgeClinic} // [[((Doctor "Dr. Grey" "Pediatrics"),2),((Doctor "Dr. Black" "General Practice"),2)]]
//Start = hospitalPatientCount {} // []


/*5. Special tree
Given a special kind of tree, in which only the leaves 
contain values, return a sorted list with all the values in the leaves.
*/

:: STree a = SNode (STree a) (STree a) | SLeaf a
tree1s = SNode (SNode (SLeaf 5) (SLeaf 4)) (SNode (SLeaf 3) (SNode (SLeaf 3) (SLeaf 3)))
tree2s = SNode (SLeaf 20) (SNode (SNode (SNode (SNode (SLeaf 33) (SLeaf 34)) (SLeaf 98)) (SLeaf 32)) (SLeaf 31))


Aux3 :: (STree Int) -> [Int]
Aux3  (SLeaf x) = [x]
Aux3 (SNode l r) = Aux3 l ++ Aux3 r

keepValues :: (STree Int) -> [Int]
keepValues tree = sort (Aux3 tree) 

//Start = keepValues (SLeaf 5) // [5]
//Start = keepValues tree1s // [3,3,3,4,5]
//Start = keepValues tree2s // [20,31,32,33,34,98]


/*6. Leaves both children
Given a tree, count the nodes that have Leaf in both children.	
		5
	   / \     => +1
	  L   L
	  
	  	5
	   / \     => +0
	  L   5
	
		5
	   / \     => +0
	  2   L
	
		5
	   / \     => +0
	  3   6	
*/
:: Tree a = Node a (Tree a) (Tree a)
			| Leaf

//							+1					+1							+1				  +1
tree1 = Node 6 (Node 4 (Node 2 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 8 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf))

//												  +1				  +1					+1							+1				   +1		
tree2 = Node 6 (Node 4 (Node 2 Leaf (Node 8 (Node 10 Leaf Leaf) (Node 11 Leaf Leaf))) (Node 5 Leaf Leaf)) (Node 8 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf))

//							     +1									  +1				   +1
tree3 = Node '6' (Node '4' (Node '2' Leaf Leaf) Leaf) (Node '8' (Node '7' Leaf Leaf) (Node '9' Leaf Leaf))


countNode :: (Tree a) -> Int
countNode Leaf = 0
countNode (Node x l r) = 1 + max (countNode l) (countNode r)

//Start = countNode tree1 // 4
//Start = countNode tree2 // 5
//Start = countNode tree3 // 3



/*7. Person
Create the type synonym called Person, 
which is the tuple of (String,Int,String) 
representing the name, age and the occupation.
Given an array of Person, write a function that calculates 
the average age of the array in Real.
*/

:: Person :== (String, Int, String)

avgAge :: {Person} -> Real
avgAge list 
| isEmpty ls = 0.0
= toReal (sum [snd3 el \\ el <- ls]) / toReal (length ls)
where ls = [el \\ el <-: list]


//Start = avgAge {("PersonA",5,"Occ1"),("PersonB",10,"Occ2"),("PersonC",22,"Occ3")} // 12.3333333333333
//Start = avgAge {("PersonA",5,"Occ1"),("PersonB",15,"Occ2"),("PersonC",10,"Occ3")} // 10
//Start = avgAge {} // 0
//Start = avgAge {("PersonA", 0, "Occ1"), ("PersonB", 0, "Occ2")} // 0



/*8. GetBool
Implement a function that takes a string as input and 
returns a list of Boolean values based on the information 
extracted from the input string.	
The input string consists of characters representing 
Boolean values, where "T" or "True" represents True,
and "F" or "False" represents False.	
*/

GetTF ::  String -> [Bool]
GetTF str = [ if (el=='F') (False) (True) \\ el <-: str ]
	
//Start = GetTF "TFTFTFTFTFTFTFTF" //[True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False]
//Start = GetTF "TTTTTFTTTTT" //[True,True,True,True,True,False,True,True,True,True,True]
//Start = GetTF "TrueTrueTrueTrueTrueFalse" //[True,True,True,True,True,False]
//Start = GetTF "FalseFalseFalseFalseTrue" //[False,False,False,False,True]
//Start = GetTF "FalseFalseTT" // [False,False,True,True]
//Start = GetTF "" // []



/*9. Flipping
Write a function that takes a String and creates a new string by flipping 
the characters. Flipping means replacing the character at ith index in the 
alphabet with 26-i th character.
Eg: a becomes z, b becomes y, c becomes x and so on.
Assume the input is lowercase string and whitespaces (they are not replaced).
*/

Aux5 :: Char -> Char
Aux5 char 
| isLower char = toChar (toInt 'z' - (toInt char - toInt 'a'))
= char

fx :: String -> String
fx str = { if (el==' ') (' ') (Aux5 el) \\ el <-: str }

//Start = fx "abcdefghijklmnopqrstuvwxyz" // "zyxwvutsrqponmlkjihgfedcba"
//Start = fx "hello john how are you" // "svool qlsm sld ziv blf"
//Start = fx "svool qlsm sld ziv blf" // "hello john how are you"
//Start = fx "" // ""
//Start = fx "   abc  " // "   zyx  "


/*10. Refactoring
You are assigned to refactor Python code to Java, that is
changing the variable names from snake_case to camelCase. 
Write a function for this conversion.
You can assume the _ will only occur between words.
Example: variable_one -> variableOne 
*/

Aux4 :: [Char] -> [Char]
Aux4 [] = []
Aux4 [x:xs]
| x == '_' = [toUpper (hd xs): Aux4 (tl xs)]
= [x: Aux4 xs]
Aux4 [x,y:xs]
| x == '_' = [toUpper y: Aux4 xs]
= [x: Aux4 [y:xs]]

conversion :: String -> String
conversion str = toString (Aux4 List)
where List = [ el \\ el <-: str]

//Start = conversion "variable_one" // "variableOne"
//Start = conversion "lazy_name_g" // "lazyNameG"
//Start = conversion "" // ""



/*11. Valid username
Given a string, define a function which returns True if the username 
is valid and False otherwise. The username has the following rules:
- must contain only alphanumeric characters (letters a-z, digits 0-9, and underscore _).
- must be between 4 and 20 characters in length, inclusive.
- must not contain consecutive underscores ("__").
*/

lsABC = ['a'..'z'] ++ ['0'..'9'] ++ ['_']

occ :: Char String -> Int
occ char str = length [el \\ el <-: str | char == el ]

validateUsername :: String -> Bool
validateUsername str = (occ '_' str == 1 || occ '_' str == 0) && and [ isMember el lsABC \\ el <-: str] && (size str >= 4 && size str <= 20)
where ls = [el \\ el <-: str]


//Start = validateUsername "user123"     // True
//Start = validateUsername "user_name"   // True
//Start = validateUsername "___user"     // False
//Start = validateUsername "user__name"  // False
//Start = validateUsername "user"        // True
//Start = validateUsername "username1234567890123456789" // False
//Start = validateUsername "user123_"    // True
//Start = validateUsername "user__name_" // False


