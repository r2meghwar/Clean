module PT8
import StdEnv

:: University = Elte | Corvinus | BME

:: Student={
			id::Int
           ,uni::University
           ,grades::[Int]
           }

arrSt = {
			{ id = 1, uni = Elte, grades = [4,3,5,2,1,5] },	// failed 1 subject, (1,1)
			{ id = 2, uni = BME, grades = [1,2,3,1,1] },    // failed 2 subjects, (2,3)
			{ id = 3, uni = Corvinus, grades = [2,3,5] },   // nothing failed, (3,0)
			{ id = 4, uni = BME, grades = [] },             // nothing failed, (4,0)
			{ id = 5, uni = Elte, grades = [1,2,3,1,1] },   // failed 3 subjects, (5,3)
			{ id = 6, uni = Corvinus, grades = [1] }		// failed 1 subjects, (6,1)
		}

/*
	The array of students (id, uni, grades) is given,
	
	Student.grades is the list of integer, each integer representing the grade for each subject.
	
	there are 5 possible grades in each subject, 1 to 5 and grade 1 means 'fail'.
	
	Return the list of 2 element tuple (id, count of fail subjects).
	
	{ id = 1, uni = Elte, grades = [4,3,5,2,1,5] } => this student has failed only 1 subject and id is 1, so (1,1)
	{ id = 2, uni = BME, grades = [1,2,3,1,1] } => this student has failed 3 subjects and id is 2, so (2,3)

*/

count :: [Int] -> Int
count grades = length [grade \\ grade <- grades | grade == 1]

whoFailed :: {Student} -> [(Int,Int)]
whoFailed students = [ (student.id, count student.grades) \\ student <-: students]

//Start = whoFailed arrSt // [(1,1),(2,3),(3,0),(4,0),(5,3),(6,1)]
