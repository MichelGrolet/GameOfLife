import java.util.NoSuchElementException

object JeuDeLaVie {
	type Grille = List[(Int, Int)]

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

		moteurJDLV(l, 5)
		//moteurFredkin(l, 5)
		//moteurVariante(l, 5)
	}

	def lignes(l: List[String], ligne: Int): Grille = l match {
		case Nil => Nil
		case t :: q => colonnes(t, ligne, 0) ++ lignes(q, ligne + 1)
	}

	def colonnes(l: String, ligne: Int, col: Int): Grille = l match {
		case "" => Nil
		case l => if (l.head == 'X') (ligne, col) :: colonnes(l.tail, ligne, col + 1)
		else colonnes(l.tail, ligne, col + 1)
	}

	def chainesToGrille(l: List[String]): Grille = lignes(l, 0)

	//q2
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

	//q3
	def voisines8(l: Int, c: Int): List[(Int, Int)] = (l - 1, c - 1) :: (l - 1, c) :: (l - 1, c + 1) :: (l, c - 1) :: (l, c + 1) :: (l + 1, c - 1) :: (l + 1, c) :: (l + 1, c + 1) :: Nil

	//q4
	def listeVoisinesVivantes(a: Int, b: Int, g: Grille): Grille = voisines8(a, b) filter ((va, vb) => g.contains((va, vb)))

	def survivantes(g: Grille): Grille =
		g filter ((ord, abs) =>
			val length = listeVoisinesVivantes(ord, abs, g).length
			length ==
			2 || length == 3)

	//q5
	def listeVoisinesMortes(a: Int, b: Int, g: Grille): Grille = voisines8(a, b) filter ((va, vb) => (!g.contains((va, vb))))

	def candidates(g: Grille): Grille =
		(g foldLeft List.empty) ((acc, elem) => acc ++ listeVoisinesMortes(elem._1, elem._2, g))


	//q6
	def naissances(g: Grille): Grille = candidates(g) filter ((ord, abs) => listeVoisinesVivantes(ord, abs, g).length == 3)

	//q7
	def concat(a: Grille, b: Grille): Grille = (a foldLeft b) ((acc, elem) => if (!acc.contains(elem)) acc ++ List(elem) else acc)

	def jeuDeLaVie(init: Grille, n: Int): Unit =
		if (n > 0) {
			afficherGrille(init)
			println("\n-=--=--=--=--=--=--=--=--=-\n")
			jeuDeLaVie(concat(naissances(init), survivantes(init)), n - 1)
		}

	/**
	 * Q12 : jeu de la vie
	 *
	 * @param init Grille initiale
	 * @param n    nombre d'itérations
	 */
	def moteurJDLV(init: Grille, n: Int) = moteur(init, n, naitJDLV, survitJDLV, voisines8)

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
	 * Q12 : fredkin
	 *
	 * @param init Grille initiale
	 * @param n    nombre d'itérations
	 */
	def moteurFredkin(init: Grille, n: Int) = moteur(init, n, naitF, survitF, voisines4)

	/**
	 * Q8
	 *
	 * @param l ligne concernée
	 * @param c colonne concernée
	 * @return liste des coordonnées des 4 voisines
	 */
	def voisines4(l: Int, c: Int): List[(Int, Int)] = (l - 1, c) :: (l, c - 1) :: (l, c + 1) :: (l + 1, c) :: Nil

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
	 * Q13
	 *
	 * @param l ligne concernée
	 * @param c colonne concernée
	 * @return liste des 4 voisines en diagonale
	 */
	def voisines4variante(l: Int, c: Int): List[(Int, Int)] = (l - 1, c - 1) :: (l - 1, c + 1) :: (l + 1, c - 1) :: (l + 1, c + 1) :: Nil
}

