module PTExtra2
import StdEnv

:: University = Elte | Corvinus | BME

:: Student={id::Int
           ,uni::University
           ,grades::[Int]}
														// avg of grades (just for testing purpose)
stu1 = { id = 1, uni = Elte, grades = [4,3,5,2,1,5] } 	// 3.33
stu2 = { id = 2, uni = BME, grades = [1,2,3,5,1] } 		// 2.4
stu3 = { id = 3, uni = Corvinus, grades = [2,3,5] } 	// 3.33
stu4 = { id = 4, uni = BME, grades = [] }				// 0.0
stu5 = { id = 5, uni = Elte, grades = [1,2,3,1,1]}  	// 1.6 
stu6 = { id = 6, uni = Corvinus, grades = [1] } 		// 1

/*
 1. Overload < operator for Student,
	Student A is less than student B if A has less average grade than B and the id of A is less than the id of B.
*/

instance < Student
where
	(<) a b = if (not (isEmpty a.grades) && not (isEmpty b.grades)) (avg (a.grades) < avg (b.grades)) True

//Start = stu2 < stu3 // True
//Start = stu1 < stu2 // False
//Start = stu1 < stu3 // False
//Start = stu4 < stu5 // True
