import java.util.NoSuchElementException

object JeuDeLaVie {
	def main(args: Array[String]): Unit = {
		val l = chainesToGrille(List(
			"  X  ",
			" XXX ",
			"XX XX",
			" XXX ",
			"  X  "
		))
		val l2 = chainesToGrille(List(
			" X ",
			"  X",
			"XXX"
		))

		jeuDeLaVie(l, 10)
		//jdlv
		moteur(l, 10, naitJDLV, survitJDLV, voisines8)
		moteur(l, 10, nait)
	}

	type Grille = List[(Int,Int)]

	/*
		def lignes(l:List[String],ligne:Int):Grille=l match{
  			case Nil=>Nil
  			case t::q=>colonnes(t,ligne,0)++lignes(q,ligne+1)
  		}

  		def colonnes(l:String,ligne:Int,col:Int):Grille=l match{
  			case ""=>Nil
  			case l=> if(l.head=='X') (ligne,col)::colonnes(l.tail,ligne,col+1)
  				else colonnes(l.tail,ligne,col+1)
  		}

		def chainesToGrille(l:List[String]):Grille=lignes(l,0)
	*/

	//q1
	def chainesToGrille(l:List[String]):Grille = {
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

	//q2
	def afficherGrille(g:Grille):Unit = {
		if (g.isEmpty)
			println("(vide)")
		else {
			// couple minimal de la grille
			val min = g reduceLeft ((a, b) =>
				val (a1, a2) = a
				val (b1, b2) = b
				(if (a1 < b1) a1 else b1,
				if (a2 > b2) b2 else a2))
			println("min : "+min)

			// couple maximal de la grille
			val max = g reduceLeft ((a, b) =>
				val (a1, a2) = a
				val (b1, b2) = b
				(if (a1 < b1) b1 else a1,
				if (a2 > b2) a2 else b2))
			println("max : "+max)

			// itère de min à max et affiche quand le couple est dans g
			// ._1 : ligne concernée
			// ._2 : colonne concernée
			def iterer(c: (Int, Int)): Unit = {
				// si il reste des lignes :
				if (c._1 <= max._1) {
					// si il reste des colonnes :
					if (c._2 <= max._2) {
						// si c est en X dans la grille :
						if (g.contains(c)) {
							print(" X ")
							//println("("+c._1+","+c._2+")")
						}
						else print("   ")
						iterer((c._1, c._2 + 1))
					} else { // ligne suivante
						println("")
						iterer((c._1 + 1, min._2))
					}
				}
			}
			println()
			iterer(min)
			println()
		}
	}

	//q3
	def voisines8(l:Int,c:Int):List[(Int, Int)] = (l-1,c-1)::(l-1,c)::(l-1,c+1)::(l,c-1)::(l,c+1)::(l+1,c-1)::(l+1,c)::(l+1,c+1)::Nil

	//q4
	def listeVoisinesVivantes(a:Int, b:Int, g:Grille):Grille = voisines8(a, b) filter ((va, vb)=>g.contains((va, vb)))
	def survivantes(g:Grille):Grille =
		g filter((ord, abs) =>
			val length = listeVoisinesVivantes(ord, abs, g).length
			length == 2 || length == 3)

	//q5
	def listeVoisinesMortes(a:Int, b:Int, g:Grille):Grille = voisines8(a, b) filter ((va, vb)=>(!g.contains((va, vb))))
	def candidates(g:Grille):Grille =
		(g foldLeft List.empty)((acc, elem)=> acc++listeVoisinesMortes(elem._1, elem._2, g))


	//q6
	def naissances(g:Grille):Grille = candidates(g) filter((ord, abs) => listeVoisinesVivantes(ord, abs, g).length == 3)

	//q7
	def concat(a: Grille, b:Grille):Grille = (a foldLeft b)((acc, elem)=> if(!acc.contains(elem)) acc ++ List(elem) else acc)

	def jeuDeLaVie(init:Grille, n:Int):Unit =
		if (n>0) {
			afficherGrille(init)
			println(init)
			println("\n-=--=--=--=--=--=--=--=--=-\n")
			jeuDeLaVie(concat(naissances(init), survivantes(init)), n-1)
		}

	//q8
	def voisines4(l:Int,c:Int):List[(Int, Int)] = (l-1,c)::(l,c-1)::(l,c+1)::(l+1,c)::Nil

	//q9 fredkin
	def naitJDLV(nbVoisines:Int):Boolean = nbVoisines==3
	def survitJDLV(nbVoisines:Int):Boolean = nbVoisines==2||nbVoisines==3

	def naitF(nbVoisines:Int):Boolean = nbVoisines%2 == 1
	def survitF(nbVoisines:Int):Boolean = nbVoisines%2 == 1

	def survivantesG(g:Grille, nait:Int=>Boolean, survit:Int=>Boolean, voisines:(Int,Int)=>List[(Int, Int)]):Grille =
		g filter((a, b) => survit((voisines(a, b) filter ((va, vb)=>g.contains((va, vb)))).length))

	def candidatesG(g:Grille, nait:Int=>Boolean, survit:Int=>Boolean, voisines:(Int,Int)=>List[(Int, Int)]):Grille =
		(g foldLeft List.empty)((acc, elem)=> acc++(voisines(elem._1, elem._2) filter ((va, vb)=>(!g.contains((va, vb))))))

	def naissancesG(g:Grille, nait:Int=>Boolean, survit:Int=>Boolean, voisines:(Int,Int)=>List[(Int, Int)]):Grille =
		candidatesG(g, nait, survit, voisines) filter((ord, abs) => nait((voisines(ord, abs) filter ((va, vb)=>g.contains((va, vb)))).length))

	//q11
	def moteur(init:Grille, n:Int, nait:Int=>Boolean, survit:Int=>Boolean, voisines:(Int,Int)=>List[(Int, Int)]):Unit =
		if (n>0) {
			afficherGrille(init)
			println(init)
			println("\n-=--=--=--=--=--=--=--=--=-\n")
			jeuDeLaVie(concat(naissancesG(init, nait, survit, voisines), survivantesG(init, nait, survit, voisines)), n-1)
		}

	//q8
	def voisines4(l:Int,c:Int):List[(Int, Int)] =
		(l-1,c)::(l,c-1)::(l,c+1)::(l+1,c)::Nil
}

