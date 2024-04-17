module SampleMidterm1
import StdEnv


/**1
  * Write a function, that takes a list of functions, and a list of

  * tuples (Int, Int) where the first Int indicates which function to

  * use and the second Int acts as a parameter and returns a list of

  * the results.
  
  * For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
  */



Router :: [(a->b)] [(Int,a)] -> [b]
Router listFunc listRoute
| isEmpty listFunc || isEmpty listRoute = []
= [ (listFunc!!(funcNum-1)) param\\ (funcNum, param) <-listRoute]


//Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)] //[True, False, False]
//Start = Router [((+)1),((*)2),((^)2),((rem) 100)] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[9,46,32,1337,8]
//Start = Router [(\x = [1..x]),(\x = [n\\n<-[1..x]|x rem n ==0]),(\x = [x,x*2..x*10])] [(2,36),(1,13),(3,5),(2,128),(3,1)]
//[[1,2,3,4,6,9,12,18,36],[1,2,3,4,5,6,7,8,9,10,11,12,13],[5,10,15,20,25,30,35,40,45,50],[1,2,4,8,16,32,64,128],[1,2,3,4,5,6,7,8,9,10]]
//Start = Router [] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[]
//Start = Router [isEven, isOdd] [] //[]
