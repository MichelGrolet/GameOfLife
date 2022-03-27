import java.util.NoSuchElementException

object JeuDeLaVie {
	type Grille = List[(Int, Int)]

	def main(args: Array[String]): Unit = {
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
		
		assert(t4===List((0,2),(1,1),(1,2),(1,3),(2,0),(2,1),(2,3),(2,4),(3,1),(3,2),(3,3),(4,2)))
		
	
	//Test q1 : 2
		
		assert(t4===List((0,1),(1,2),(2,0),(2,1),(2,2),(3,2)))
		

	//Test q1 : 3
		
		assert(t4===List())
		

	//Test q1 : 4
		
		assert(t4===List((2,31)))
		


	//Q3
	
	val vois81=voisines8(1,2);
	val vois82=voisines8(-2,5);

	//Test q3 : 1
		
    	assert(vois81===List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3)))
  		

	//Test q3 : 2
		
    	assert(vois82===List((-3,4),(-3,5),(-3,6),(-2,4),(-2,6),(-1,4),(-1,5),(-1,6)))
  		

		
	//Q4
	
	val surv1=List((1,1));
	val surv2=List((1,1),(1,2),(1,3),(2,2));

	//Test q4 : 1
		
    	assert(survivantes(surv1)===List())
  		

	//Test q4 : 2
		
    	assert(survivantes(surv2)===List((1,2)))
  		


	//Q5
	
	val cand1=List((1,2));
	val cand2=List((1,2),(2,3));

	//Test q5 : 1
		
    	assert(candidates(cand1))===List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3))
  		

	//Test q5 : 2
		
    	assert(candidates(cand2))===List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3),(1,4),(2,3),(2,4),(3,2),(3,3),(3,4))
  		

	//Q6
	
	val nais1=List((1,2));
	val nais2=List((1,1),(1,2),(1,3));

	//Test q6 : 1
		
    	assert(naissances(nais1))===List()
  		

	//Test q6 : 2
		
    	assert(naissances(nais2))===List((0,2),(2,2))
  		


	//Q8
	
	val vois41=voisines4(1,2);
	val vois42=voisines4(-2,5);

	//Test q8 : 1
		
    	assert(vois41===List((0,2),(1,1),(1,3),(2,2)))
  		

	//Test q8 : 2
		
    	assert(vois42===List((-3,5),(-2,4),(-2,6),(-1,5)))
  		

	//Q9
	
	val vois41=voisines4(1,2);
	val vois42=voisines4(-2,5);

	//Test q9 : 1
		
    	assert(naitJDLV(2)===false)
  		

	//Test q9 : 2
		
    	assert(naitJDLV(3)===true)
  		

	//Test q9 : 3
		
    	assert(survitJDLV(1)===false)
  		
	
	//Test q9 : 4
		
    	assert(survitJDLV(2)===true)
  		

	//Test q9 : 5
		
    	assert(survitJDLV(5)===false)
  			

	//Test q9 : 6
		
    	assert(naitF(2)===false)
  		

	//Test q9 : 7
		
    	assert(naitF(3)===true)
  		

	//Test q9 : 8
		
    	assert(survitF(1)===true)
  		
	
	//Test q9 : 9
		
    	assert(survitF(2)===false)
  		

	
	//Q10

	val survG1=List((1,2));
	val survG2=List((1,1),(1,2),(1,3));
		
	//Test q10 : 1
		
    	assert(survivantesG(survG1,survitJDLV,voisines8)===List())
  		

	//Test q10 : 2
		
    	assert(survivantesG(survG1,survitF,voisines4)===List())
  		

	//Test q10 : 3
		
    	assert(survivantesG(survG2,survitJDLV,voisines8)===List((1,2)))
  		

	//Test q10 : 4
		
    	assert(survivantesG(survG2,survitF,voisines4)===List())
  		

	//Test q10 : 5
		
    	assert(candidatesG(survG1,voisines8)===List((0,1),(0,2),(0,3),(1,1),(1,3),(2,1),(2,2),(2,3)))
  		

	//Test q10 : 6
		
    	assert(candidatesG(survG1,voisines4)===List((0,2),(1,1),(1,3),(2,2)))
  		

	//Test q10 : 7
		
    	assert(naissancesG(survG1,naitJDLV,voisines8)===List())
  		

	//Test q10 : 8
		
    	assert(naissancesG(survG1,naitF,voisines4)===List())
  		

	//Test q10 : 9
		
    	assert(naissancesG(survG2,naitJDLV,voisines8)===List((0,2),(2,2)))
  		

	//Test q10 : 10
		
    	assert(naissancesG(survG2,naitF,voisines4)===List())
  		
	}


	/**
	 * Q1 : fonction auxiliaire 1
	 *
	 * @param l     Liste de chaines
	 * @param ligne Numero de ligne courante
	 */
	def lignes(l: List[String], ligne: Int): Grille = l match {
		case Nil => Nil
		case t :: q => colonnes(t, ligne, 0) ++ lignes(q, ligne + 1)
	}


	/**
	 * Q1 : fonction auxiliaire 2
	 *
	 * @param l     Chaine a traiter
	 * @param ligne Numero de ligne courante
	 * @param col   Numero de colonne courante
	 */
	def colonnes(l: String, ligne: Int, col: Int): Grille = l match {
		case "" => Nil
		case l => if (l.head == 'X') (ligne, col) :: colonnes(l.tail, ligne, col + 1)
		else colonnes(l.tail, ligne, col + 1)
	}

	/**
	 * Q1 : chainesToGrille
	 * Utilise les fonctions auxiliaires lignes et colonnes
	 *
	 * @param l    Liste de chaines
	 * @param n    nombre d'iterations
	 */
	def chainesToGrille(l: List[String]): Grille = lignes(l, 0)

	/**
	 * Q2 : afficherGrille
	 * Affiche une grille
	 *
	 * @param g   Grille a afficher
	 */
	def afficherGrille(g: Grille): Unit = {
		if (g.isEmpty)
			println("(vide)")
		else {
			// couple minimal de la grille
			val min = g reduceLeft ((a, b) =>
				val (a1, a2) = a
				val (b1, b2) = b
				(if (a1 < b1) a1 else b1,
					if (a2 > b2) b2 else a2))

			// couple maximal de la grille
			val max = g reduceLeft ((a, b) =>
				val (a1, a2) = a
				val (b1, b2) = b
				(if (a1 < b1) b1 else a1,
					if (a2 > b2) a2 else b2))

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

	/**
	 * Q3 : voisines8
	 * Donne les 8 voisines d'une cellule de la grille
	 *
	 * @param l    Numero de ligne
	 * @param c    Numero de colonne
	 */
	def voisines8(l: Int, c: Int): List[(Int, Int)] = (l - 1, c - 1) :: (l - 1, c) :: (l - 1, c + 1) :: (l, c - 1) :: (l, c + 1) :: (l + 1, c - 1) :: (l + 1, c) :: (l + 1, c + 1) :: Nil

	/**
	 * Q4 : fonction auxiliaire
	 *
	 * @param a    Numero de ligne
	 * @param b    Numero de colonne
	 * @param g    Grille a traiter
	 */
	def listeVoisinesVivantes(a: Int, b: Int, g: Grille): Grille = voisines8(a, b) filter ((va, vb) => g.contains((va, vb)))

	/**
	 * Q4 : survivantes
	 * Donne les survivantes a la prochaine iteration
	 *
	 * @param g   Grille a traiter
	 */
	def survivantes(g: Grille): Grille =
		g filter ((ord, abs) =>
			val length = listeVoisinesVivantes(ord, abs, g).length
			length ==
			2 || length == 3)

	/**
	 * Q5 : fonction auxiliaire
	 *
	 * @param a    Numero de ligne
	 * @param b    Numero de colonne
	 * @param g    Grille a traiter
	 */
	def listeVoisinesMortes(a: Int, b: Int, g: Grille): Grille = voisines8(a, b) filter ((va, vb) => (!g.contains((va, vb))))

	/**
	 * Q5 : survivantes
	 * Donne les candidates pour une naissance
	 *
	 * @param g   Grille a traiter
	 */
	def candidates(g: Grille): Grille =
		(g foldLeft List.empty) ((acc, elem) => acc ++ listeVoisinesMortes(elem._1, elem._2, g))


	/**
	 * Q6 : naissances
	 * Donne les naissances pour l'iteration
	 *
	 * @param g   Grille a traiter
	 */
	def naissances(g: Grille): Grille = candidates(g) filter ((ord, abs) => listeVoisinesVivantes(ord, abs, g).length == 3)

	/**
	 * Q7 : fonction auxiliaire
	 * Concatene
	 *
	 * @param a   Grille 1 a traiter
	 * @param b   Grille 2 a traiter
	 */
	def concat(a: Grille, b: Grille): Grille = (a foldLeft b) ((acc, elem) => if (!acc.contains(elem)) acc ++ List(elem) else acc)

	/**
	 * Q7 : jeudelavie
	 * Deroule le jeu a l'iteration donnee
	 *
	 * @param init Grille de depart
	 * @param n    Nombre d'iterations
	 */
	def jeuDeLaVie(init: Grille, n: Int): Unit =
		if (n > 0) {
			afficherGrille(init)
			println("\n-=--=--=--=--=--=--=--=--=-\n")
			jeuDeLaVie(concat(naissances(init), survivantes(init)), n - 1)
		}

	/**
	 * Q8
	 *
	 * @param l ligne concernée
	 * @param c colonne concernée
	 * @return liste des coordonnées des 4 voisines
	 */
	def voisines4(l: Int, c: Int): List[(Int, Int)] = (l - 1, c) :: (l, c - 1) :: (l, c + 1) :: (l + 1, c) :: Nil

	/**
	 * Q9
	 *
	 * @param nbVoisines nombre de voisines à vérifier
	 * @return booléen indiquant si la case naît
	 */
	def naitJDLV(nbVoisines: Int): Boolean = nbVoisines == 3

	/**
	 * Q9
	 *
	 * @param nbVoisines nombre de voisines à vérifier
	 * @return booléen indiquant si la case survit
	 */
	def survitJDLV(nbVoisines: Int): Boolean = nbVoisines == 2 || nbVoisines == 3

	/**
	 * Q10
	 *
	 * @param g        grille concernée
	 * @param survit   fonction de survie
	 * @param voisines fonction des voisines
	 * @return grille des survivantes
	 */
	def survivantesG(g: Grille, survit: Int => Boolean, voisines: (Int, Int) => List[(Int, Int)]): Grille =
		g filter ((a, b) => survit((voisines(a, b) filter ((va, vb) => g.contains((va, vb)))).length))

	/**
	 * Q10
	 *
	 * @param g        grille concernée
	 * @param nait     fonction des naissances
	 * @param voisines liste des voisines
	 * @return liste des naissances
	 */
	def naissancesG(g: Grille, nait: Int => Boolean, voisines: (Int, Int) => List[(Int, Int)]): Grille =
		candidatesG(g, voisines) filter ((ord, abs) => nait((voisines(ord, abs) filter ((va, vb) => g.contains((va, vb)))).length))

	/**
	 * Q10
	 *
	 * @param g        grille concernée
	 * @param voisines fonction des voisines
	 * @return grille des candidates
	 */
	def candidatesG(g: Grille, voisines: (Int, Int) => List[(Int, Int)]): Grille =
		(g foldLeft List.empty) ((acc, elem) => acc ++ (voisines(elem._1, elem._2) filter ((va, vb) => (!g.contains((va, vb))))))

	/**
	 * Q11
	 *
	 * @param init     grille initiale
	 * @param n        nombre d'itérations à afficher
	 * @param nait     règles de naissance
	 * @param survit   règles de survie
	 * @param voisines règles des voisines
	 */
	def moteur(init: Grille, n: Int, nait: Int => Boolean, survit: Int => Boolean, voisines: (Int, Int) => List[(Int, Int)]): Unit = {
		if (n > 0) {
			afficherGrille(init)
			println("\n-=--=--=--=--=--=--=--=--=-\n")
			jeuDeLaVie(concat(naissancesG(init, nait, voisines), survivantesG(init, survit, voisines)), n - 1)
		}
	}

	/**
	 * Q12 : jeu de la vie
	 *
	 * @param init Grille initiale
	 * @param n    nombre d'itérations
	 */
	def moteurJDLV(init: Grille, n: Int) = moteur(init, n, naitJDLV, survitJDLV, voisines8)

	/**
	 * Q12 : fredkin
	 *
	 * @param init Grille initiale
	 * @param n    nombre d'itérations
	 */
	def moteurFredkin(init: Grille, n: Int) = moteur(init, n, naitF, survitF, voisines4)

	/**
	 * Q13 : fredkin variante
	 *
	 * @param init grille initiale
	 * @param n    nombre d'itérations
	 */
	def moteurVariante(init: Grille, n: Int) = moteur(init, n, naitF, survitF, voisines4variante)

	def naitF(nbVoisines: Int): Boolean = nbVoisines % 2 == 1

	def survitF(nbVoisines: Int): Boolean = nbVoisines % 2 == 1

	/**
	 * Q13
	 *
	 * @param l ligne concernée
	 * @param c colonne concernée
	 * @return liste des 4 voisines en diagonale
	 */
	def voisines4variante(l: Int, c: Int): List[(Int, Int)] = (l - 1, c - 1) :: (l - 1, c + 1) :: (l + 1, c - 1) :: (l + 1, c + 1) :: Nil
}

