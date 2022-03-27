import org.scalatest._
import JeuDeLaVie._

class TestChassardGrolet extends FunSuite{
	//Q1

	val t1 = chainesToGrille(List(
		"  X  ",
		" XXX ",
		"XX XX",
		" XXX ",
		"  X  "
	))
		
	val t2 = chainesToGrille(List(
		" X ",
		"  X",
		"XXX",
		"  X"
	))
		
	val t3 = chainesToGrille(List(
		"  ",
		"          "
	))

	val t4 = chainesToGrille(List(
		"  ",
		"          ",
		"                               X     "
	))

	//Test q1 : 1
		test("t1 doit etre List((0,2),(1,1),(1,2),(1,3),(2,0),(2,1),(2,3),(2,4),(3,1),(3,2),(3,3),(4,2))") {
				assert(t4===List((0,2),(1,1),(1,2),(1,3),(2,0),(2,1),(2,3),(2,4),(3,1),(3,2),(3,3),(4,2)))
		}
	
	//Test q1 : 2
		test("t2 doit etre List((0,1),(1,2),(2,0),(2,1),(2,2),(3,2))") {
			assert(t4===List((0,1),(1,2),(2,0),(2,1),(2,2),(3,2)))
		}

	//Test q1 : 3
		test("t3 doit etre List()") {
				assert(t4===List())
		}

	//Test q1 : 4
		test("t4 doit etre List((2,31))") {
				assert(t4===List((2,31)))
		}


	//Q3
	
	val vois81=voisines8(1,2);
	val vois82=voisines8(-2,5);

	//Test q3 : 1
		test("vois81 doit etre List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3))") {
    		assert(vois81===List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3)))
  		}

	//Test q3 : 2
		test("vois82 doit etre List((-3,4),(-3,5),(-3,6),(-2,4),(-2,6),(-1,4),(-1,5),(-1,6))") {
    		assert(vois82===List((-3,4),(-3,5),(-3,6),(-2,4),(-2,6),(-1,4),(-1,5),(-1,6)))
  		}

		
	//Q4
	
	val surv1=List((1,1));
	val surv2=List((1,1),(1,2),(1,3),(2,2));

	//Test q4 : 1
		test("surv1 doit etre List()") {
    		assert(survivantes(surv1)===List())
  		}

	//Test q4 : 2
		test("surv2 doit etre List((1,2))") {
    		assert(survivantes(surv2)===List((1,2)))
  		}


	//Q5
	
	val cand1=List((1,2));
	val cand2=List((1,2),(2,3));

	//Test q5 : 1
		test("cand1 doit etre List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3))") {
    		assert(candidates(cand1))===List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3))
  		}

	//Test q5 : 2
		test("cand2 doit etre List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3),(1,4),(2,3),(2,4),(3,2),(3,3),(3,4))") {
    		assert(candidates(cand2))===List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3),(1,4),(2,3),(2,4),(3,2),(3,3),(3,4))
  		}

	//Q6
	
	val nais1=List((1,2));
	val nais2=List((1,1),(1,2),(1,3));

	//Test q6 : 1
		test("nais1 doit etre List()") {
    		assert(naissances(nais1))===List()
  		}

	//Test q6 : 2
		test("nais2 doit etre List((0,2),(2,2))") {
    		assert(naissances(nais2))===List((0,2),(2,2))
  		}


	//Q8
	
	val vois41=voisines4(1,2);
	val vois42=voisines4(-2,5);

	//Test q8 : 1
		test("vois41 doit etre List((0,2),(1,1),(1,3),(2,2))") {
    		assert(vois41===List((0,2),(1,1),(1,3),(2,2)))
  		}

	//Test q8 : 2
		test("vois42 doit etre List((-3,5),(-2,4),(-2,6),(-1,5))") {
    		assert(vois42===List((-3,5),(-2,4),(-2,6),(-1,5)))
  		}

	//Q9
	
	val vois41=voisines4(1,2);
	val vois42=voisines4(-2,5);

	//Test q9 : 1
		test("doit etre faux)") {
    		assert(naitJDLV(2)===false)
  		}

	//Test q9 : 2
		test("doit etre vrai") {
    		assert(naitJDLV(3)===true)
  		}

	//Test q9 : 3
		test("doit etre faux") {
    		assert(survitJDLV(1)===false)
  		}
	
	//Test q9 : 4
		test("doit etre vrai") {
    		assert(survitJDLV(2)===true)
  		}

	//Test q9 : 5
		test("doit etre faux") {
    		assert(survitJDLV(5)===false)
  		}	

	//Test q9 : 6
		test("doit etre faux)") {
    		assert(naitF(2)===false)
  		}

	//Test q9 : 7
		test("doit etre vrai") {
    		assert(naitF(3)===true)
  		}

	//Test q9 : 8
		test("doit etre vrai") {
    		assert(survitF(1)===true)
  		}
	
	//Test q9 : 9
		test("doit etre false") {
    		assert(survitF(2)===false)
  		} 

	
	//Q10

	val survG1=List((1,2));
	val survG2=List((1,1),(1,2),(1,3));
		
	//Test q10 : 1
		test("doit etre List()") {
    		assert(survivantesG(survG1,survitJDLV,voisines8)===List())
  		}

	//Test q10 : 2
		test("doit etre List())") {
    		assert(survivantesG(survG1,survitF,voisines4)===List())
  		}

	//Test q10 : 3
		test("doit etre List((1,2))") {
    		assert(survivantesG(survG2,survitJDLV,voisines8)===List((1,2)))
  		}

	//Test q10 : 4
		test("doit etre List())") {
    		assert(survivantesG(survG2,survitF,voisines4)===List())
  		}

	//Test q10 : 5
		test("doit etre List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3))") {
    		assert(candidatesG(survG1,voisines8)===List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3)))
  		}

	//Test q10 : 6
		test("doit etre List())") {
    		assert(candidatesG(survG1,voisines4)===List((0,2),(1,1),(1,3),(2,2)))
  		}

	//Test q10 : 7
		test("doit etre List()") {
    		assert(naissancesG(survG1,naitJDLV,voisines8)===List())
  		}

	//Test q10 : 8
		test("doit etre List())") {
    		assert(naissancesG(survG1,naitF,voisines4)===List())
  		}

	//Test q10 : 9
		test("doit etre List((0,2),(2,2))") {
    		assert(naissancesG(survG2,naitJDLV,voisines8)===List((0,2),(2,2)))
  		}

	//Test q10 : 10
		test("doit etre List())") {
    		assert(naissancesG(survG2,naitF,voisines4)===List())
  		}
}