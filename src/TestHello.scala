import org.scalatest._
import JeuDeLaVie._

class TestHello extends FunSuite{
		//Test q1 : 1
		val t1 = chainesToGrille(List(
			"  X  ",
			" XXX ",
			"XX XX",
			" XXX ",
			"  X  "
		))
		
		//Test q1 : 2
		val t2 = chainesToGrille(List(
			" X ",
			"  X",
			"XXX",
			"  X"
		))
		
		//Test q1 : 3
		val t3 = chainesToGrille(List(
			"  ",
			"          "
		))
		//Test q1 : 4
		val t4 = chainesToGrille(List(
			"  ",
			"          ",
			"                               X     "
		))


	test("t1 doit etre List((0,2),(1,1),(1,2),(1,3),(2,0),(2,1),(2,3),(2,4),(3,1),(3,2),(3,3),(4,2))") {
    		assert(t4===List((0,2),(1,1),(1,2),(1,3),(2,0),(2,1),(2,3),(2,4),(3,1),(3,2),(3,3),(4,2)))
  	}
  
	test("t2 doit etre List((0,1),(1,2),(2,0),(2,1),(2,2),(3,2))") {
		assert(t4===List((0,1),(1,2),(2,0),(2,1),(2,2),(3,2)))
	}

	test("t3 doit etre List()") {
    		assert(t4===List())
  	}

	test("t4 doit etre List((2,31))") {
    		assert(t4===List((2,31)))
  	}
}







