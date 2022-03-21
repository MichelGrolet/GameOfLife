import java.util.NoSuchElementException

object JeuDeLaVie {
	def main(args: Array[String]): Unit = {
		val l = List("X  XX", "X   X", "X X X")
		val v = chaineToGrille(l)
		println(v)
		afficherGrille(v)
	}

	type Grille = List[(Int,Int)]

	def chaineToGrille(l:List[String]):Grille = {
		def coupleCroix(s:String, colonne:Int, ligne:Int, acc:Grille): Grille ={
			if(colonne < s.length){
				if(s.charAt(colonne) == 'X'){
					val paire : (Int, Int) = (ligne, colonne)
					coupleCroix(s, colonne+1, ligne, acc ++ List(paire))
				}
				else{
					coupleCroix(s, colonne+1, ligne, acc)
				}
			}
			else {
				acc
			}
		}
		// definition fonction qui permet de parcourir la liste
		def donnerCouples(liste:List[String], num:Int): Grille = liste match {
			case Nil => Nil
			case t::q if(num < liste.length) => {
				coupleCroix(liste(num), 0, num, Nil) ++ donnerCouples(liste, num+1)
			}
			case _ => Nil
		}
		donnerCouples(l, 0)
	}

	def afficherGrille(g:Grille):Unit = {
		// find minimal couple in the grid
		val min = g reduceLeft ((a,b) => {(
			if (a._1 < b._1) a._1 else b._1,
			if (a._2 >= b._2) b._2 else a._2
		)})

		val max = g reduceLeft ((a,b) => {(
			if (a._1 < b._1) b._1 else a._1,
			if (a._2 > b._2) a._2 else b._2
		)})

		// cherche c dans g
		def grilleContient(c:(Int,Int)): Boolean = {
			(g filter (curr => curr==c)) != Nil
		}

		// itère de min à max et affiche quand le couple est dans g
		// ._1 : ligne concernée
		// ._2 : colonne concernée
		def iterer(c:(Int,Int)): Unit = {
			// si il reste des lignes :
			if (c._1<=max._1) {
				// si il reste des colonnes :
				if (c._2<=max._2) {
					// si c est en X dans la grille :
					if (grilleContient(c)) print("X")
					else print(" ")
					iterer((c._1, c._2+1))
				} else { // ligne suivante
					println("")
					iterer((c._1+1, 0))
				}
			}
		}

		iterer(min)
	}


	/*def afficherGrille(g:Grille):String=g match{
	case Nil => ""
	case (a,b)::q =>
		def aux(i:Int):Unit={
			if(i<=0) print("X")
			else{
				print(" ")
				aux(i-1)
			}
		}
		aux(b)
		println()
		afficherGrille(q)
	}*/

	//q3

def voisines8(l:Int,c:Int):List[(Int,Int)]={
	val g=Grille();
	g::(l-1,c-1)::(l-1,c)::(l-1,c+1)::(l,c-1)::(l,c+1)::(l+1,c-1)::(l+1,c)::(l+1,c+1)
	
}

//q4

def survivantes(g:Grille):Grille=g match{
	case Nil=>Nil
	case t::q=>
		val voisines=compte(g,voisines8(t._1,t._2))
		if(voisines>2 && voisines<3){
			t::survivantes(q)
		}
		else survivantes(q)
}

def compte(g:Grille,vois:Grille):Int=vois match{
	case Nil=>Nil
	case t::q=>
		val l=g filter(x=>x==t)
		if(l!=Nil) 1+compte(g,q)
		else compte(g,q)
}
}