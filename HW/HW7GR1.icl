module HW7GR1
import StdEnv

// REMEMBER : COPYING OR GIVING A COPY MEANS ZERO POINTS

/* When you submit this homework, change the filename to YourFullNameHW6GR1 ( e.g JohnSmithHW5GR1)
Also, don't forget to change filename in the first line of file  */

//Please write your neptun code here: 

/* Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately.
After completing homework, upload icl file to teams.
*/

/*
Task 1

You are provided with a text where every word is written in all uppercase letters. 
Your task is to convert this text into a more readable format. 
You should make sure that each sentence starts with a capital letter, 
and the rest of the words are in lowercase.
Example:
Input:
"THIS IS AN EXAMPLE TEXT. IT DEMONSTRATES HOW YOUR TEXT SHOULD LOOK AFTER THE MODIFICATION."
Output:
"This is an example text. It demonstrates how your text should look after the modification."
*/

CapitalizeNewLineLetter :: String -> String
CapitalizeNewLineLetter chars = toString [if (i == 0 || i > 1 && chars.[i-2] == '.' && chars.[i-1] == ' ' ) (toUpper x) (toLower x) \\ x <-: chars & i <- [0..size chars]]

capitalizeFirst :: String -> String
capitalizeFirst str = CapitalizeNewLineLetter {x \\ x <-: str}

//Start = capitalizeFirst "THIS IS AN EXAMPLE TEXT. IT DEMONSTRATES HOW YOUR TEXT SHOULD LOOK AFTER THE MODIFICATION." // "This is an example text. It demonstrates how your text should look after the modification."
//Start = capitalizeFirst "THE SUN SETS IN THE WEST. THE MOON RISES IN THE EAST. STARS SHINE BRIGHT AT NIGHT." // "The sun sets in the west. The moon rises in the east. Stars shine bright at night."
//Start = capitalizeFirst "COMPUTER SCIENCE IS FASCINATING. ALGORITHMS SOLVE PROBLEMS EFFICIENTLY. DATA STRUCTURES ORGANIZE INFORMATION." // "Computer science is fascinating. Algorithms solve problems efficiently. Data structures organize information."




//Task 2

:: WaterAction = Shower | Faucet
:: City = Budapest | Debrecen | Szeged
:: CityRelation = Resident | Gardener

:: Citizen = {id::Int, rel::CityRelation, city::City, waterActionTaken::WaterAction}

/*
In an eco-conscious city, Aquaville, citizens are being awarded rewards based on their efforts to reduce water consumption.
The city officials want to know how many citizens were chosen from each city and how much reward they will get.

Write a function that takes a list of citizens and returns a list of tuples of the form (City, count, TotalReward),
where count is the number of citizens from that city who were chosen, and TotalReward is the total amount of reward allocated to that city.

Reward is calculated as follows:
    A resident who saved water in the shower gets 30000 HUF
    A resident who saved water at the faucet gets 20000 HUF
    A gardener who saved water in the shower gets 40000 HUF
    A gardener who saved water at the faucet gets 30000 HUF
*/

// Some test data

citizen1 = {id=1, rel=Resident, city=Budapest, waterActionTaken=Shower}
citizen2 = {id=2, rel=Resident, city=Budapest, waterActionTaken=Faucet}
citizen3 = {id=3, rel=Resident, city=Debrecen, waterActionTaken=Shower}
citizen4 = {id=4, rel=Resident, city=Debrecen, waterActionTaken=Faucet}
citizen5 = {id=5, rel=Resident, city=Szeged, waterActionTaken=Shower}
citizen6 = {id=6, rel=Resident, city=Szeged, waterActionTaken=Faucet}
citizen7 = {id=7, rel=Gardener, city=Budapest, waterActionTaken=Shower}
citizen8 = {id=8, rel=Gardener, city=Budapest, waterActionTaken=Faucet}
citizen9 = {id=9, rel=Gardener, city=Debrecen, waterActionTaken=Shower}
citizen10 = {id=10, rel=Gardener, city=Debrecen, waterActionTaken=Faucet}
citizen11 = {id=11, rel=Gardener, city=Szeged, waterActionTaken=Shower}
citizen12 = {id=12, rel=Gardener, city=Szeged, waterActionTaken=Faucet}

instance == City
where
	(==) Budapest Budapest = True
	(==) Debrecen Debrecen = True
	(==) Szeged Szeged = True
	(==) _ _ = False

instance == CityRelation
where
	(==) Resident Resident = True
	(==) Gardener Gardener = True
	(==) _ _ = False
	
instance == WaterAction
where
	(==) Shower Shower = True
	(==) Faucet Faucet = True
	(==) _ _ = False


calculateReward :: Citizen -> Int
calculateReward citizen
| citizen.waterActionTaken == Shower && citizen.rel == Resident = 30000
| citizen.waterActionTaken == Faucet && citizen.rel == Resident = 20000
| citizen.waterActionTaken == Shower && citizen.rel == Gardener = 40000
| citizen.waterActionTaken == Faucet && citizen.rel == Gardener = 30000
= 0

countReward :: [Citizen] City -> Int
countReward citizens city = sum [calculateReward citizen \\ citizen <- citizens | citizen.city == city]

count :: [Citizen] City -> Int
count citizens c = length [1 \\ citizen <- citizens | citizen.city == c]

AwardReward :: [Citizen] -> [(City, Int, Int)]
AwardReward citizens = [(city, count citizens city, countReward citizens city) \\ citizen <- citizens & city <-[Budapest, Debrecen, Szeged]]


//Start = AwardReward [citizen1,citizen2,citizen3,citizen4,citizen5,citizen6,citizen7,citizen8,citizen9,citizen10,citizen11,citizen12] // [(Budapest,4,120000),(Debrecen,4,120000),(Szeged,4,120000)]
//Start = AwardReward [citizen7,citizen5,citizen2] // [(Budapest,2,60000),(Debrecen,0,0),(Szeged,1,30000)]


