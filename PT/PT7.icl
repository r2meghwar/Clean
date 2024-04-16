module PT7
import StdEnv

:: University = Elte | Corvinus | BME

:: Student={id::Int
           ,uni::University
           ,grades::[Int]}

arrSt = {
			{ id = 1, uni = Elte, grades = [4,3,5,2,1,5] },	// failed 1 subject
			{ id = 2, uni = BME, grades = [1,2,3,5,1] },    // failed 2 subjects
			{ id = 3, uni = Corvinus, grades = [2,3,5] },   // nothing failed
			{ id = 4, uni = BME, grades = [] },             // nothing failed
			{ id = 5, uni = Elte, grades = [1,2,3,1,1] },   // failed 3 subjects
			{ id = 6, uni = Corvinus, grades = [1] }		// failed 1 subjects 
		}

/*
	The array of students (id, uni, grades) is given,
	
	Student.grades is the list of integer, each integer representing the grade for each subject.
	
	there are 5 possible grades in each subject, 1 to 5 and grade 1 means 'fail'.
	
	Return the list of id of students who has failed at least 2 subjects (counting grades 1 in the student.grades).
	
	For example:
	{ id = 1, uni = Elte, grades = [4,3,5,2,1,5] } => this student has failed only 1 subject.
	{ id = 2, uni = BME, grades = [1,2,3,5,1] } => this student has failed 2 subject, 
												   so this id should be included in the result list.
*/

whoFailed :: {Student} -> [Int]
whoFailed studentarray = [student.id \\ student <-: studentarray | countFailed student >= 2]
 
countFailed :: Student -> Int
countFailed student = length (flatten [filter (\x = x == 1) (student.grades)])

//Start = whoFailed arrSt // [2,5]



