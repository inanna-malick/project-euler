package ninetynineproblems

object p10 {
	//from p09
	def pack(a: List[Symbol]): List[List[Symbol]] = {
		if (!a.isEmpty) {
			val (packed, next) = a.span(_ == a.head)
			if (next == Nil) List(packed)
			else packed :: pack(next)
		} else
			List(List())
	}

	/**
	 * P10 (*) Run-length encoding of a list.
	 * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
	 * Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
	 */
	def encode(a: List[Symbol]): List[(Int, Symbol)] =
		pack(a).flatMap {
			case x :: xs => (xs.length + 1, x) :: Nil
			case Nil => Nil
		}

	println("testing 10")
	assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
		List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))

	def main(a: Array[String]): Unit = {}
}

object p11 {
	/**
	 * P11 (*) Modified run-length encoding.
	 * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
	 */
	def encodeModified(a: List[Symbol]): List[Any] = {
		import p10.pack
		pack(a).flatMap {
			case x1 :: x2 :: xs => (xs.length + 2, x1) :: Nil
			case x :: xs => x :: Nil
			case Nil => Nil
		}
	}

	println("testing 11")
	val actual = encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	val expected: List[Any] = List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
	assert(actual == expected)

	def main(a: Array[String]): Unit = {}
}

object p12 {
	/**
	 * P12 (**) Decode a run-length encoded list.
	 * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
	 */

	def decode(a: List[(Int, Symbol)]): List[Symbol] = a.flatMap { case (i, s) => (1 to i).map(x => s) }

	println("testing 12")
	val actual = decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
	val expected: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
	assert(actual == expected)

	def main(a: Array[String]): Unit = {}
}

object p13 {
	/**
	 * P13 (**) Run-length encoding of a list (direct solution).
	 * Implement the so-called run-length encoding data compression method directly.
	 * I.e. don't use other methods you've written (like P09's pack); do all the work directly.
	 */
	def encodeDirect(a: List[Symbol], acc: List[(Int, Symbol)] = Nil): List[(Int, Symbol)] = a match {
		case x :: xs =>
			val (first, rest) = xs.span(_ == x); encodeDirect(rest, (first.length + 1, x) :: acc)
		case Nil => acc.reverse
	}

	println("testing 13")
	val actual = encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	val expected: List[(Int, Symbol)] = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
	assert(actual == expected)

	def main(a: Array[String]): Unit = {}
}

object p14 {
	/**
	 * P14 (*) Duplicate the elements of a list.
	 */
	def duplicate(a: List[Symbol]): List[Symbol] = a.flatMap(x => List(x, x))

	println("testing 14")
	val actual = duplicate(List('a, 'b, 'c, 'c, 'd))
	val expected: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
	assert(actual == expected)

	def main(a: Array[String]): Unit = {}
}

object p15 {
	/**
	 * P15 (**) Duplicate the elements of a list a given number of times.
	 */

	def duplicateN(i: Int, a: List[Symbol]): List[Symbol] =
		a.flatMap(x => (1 to i).map { n => x })

	println("testing 15")
	val actual = duplicateN(3, List('a, 'b, 'c, 'c, 'd))
	val expected: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
	assert(actual == expected)

	def main(a: Array[String]): Unit = {}
}

object p16 {
	/**
	 * P16 (**) Drop every Nth element from a list.
	 */
	def drop(i: Int, a: List[Symbol]): List[Symbol] = {
		def go(a: List[Symbol], n: Int=1, acc: List[Symbol] = Nil): List[Symbol] = a match {
			case x :: xs => if (n==i) go(xs, 1, acc) else  go(xs, n+1, x :: acc) 
			case _ => acc.reverse
		}
		
		go(a)
	}

	println("testing 16")
	val actual = drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	val expected: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
	assert(actual == expected)
	
	def main(a: Array[String]): Unit = {}
}


object p17 {
	/**
	 * P17 (**) Split a list into two parts.
	 * The length of the first part is given. Use a Tuple for your result.
	 */
	def split(i: Int, a: List[Symbol], pre: List[Symbol] = Nil): (List[Symbol], List[Symbol]) = a match {
		case x :: xs => if (i > 0) split(i-1, xs, x :: pre) else (pre.reverse, a)
		case _ => (pre, Nil)
	}

	println("testing 17")
	val actual = split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	val expected: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	assert(actual == expected)
	
	def main(a: Array[String]): Unit = {}
}



/**
P18 (**) Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
Example:

scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g)
 */

object p18 extends App {
	// assume k > i, i > 0
	def slice[T](i: Int, k: Int, l: List[T]): List[T] = {
		assert(i>=0)
		assert(k>i)
		if (i == 0) {
			l.take(k)
		} else l match {
			case h:: t => slice(i-1, k-1, t)
			case Nil => Nil
		}
	}
	println("testing 18")	
	val actual = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	val expected: List[Symbol] = List('d, 'e, 'f, 'g)
	assert(actual == expected)
}

/**
P19 (**) Rotate a list N places to the left.
Examples:
scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
 */
object p19 extends App {
	// assumption: i < l.length
	def rotate[T](i: Int, l: List[T]): List[T] = i match {
		case 0 => l
		case x if x > 0 => {
			val (first, rest) = l.splitAt(x)
			rest ::: first
		}
		case x if x < 0 => {
			val (first, rest) = l.splitAt(l.length - x.abs) // l.length traverses list, could be more efficient
			rest ::: first
		}
	}
	
	println("testing p19")
	val r1 = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	assert(r1 == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c), r1)
	val r2 = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	assert(r2 == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i), r2)
	
}

/**
P20 (*) Remove the Kth element from a list.
Return the list and the removed element in a Tuple. Elements are numbered from 0.
Example:

scala> removeAt(1, List('a, 'b, 'c, 'd))
res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
 */
object p20 extends App {
	
	def removeAt[T](k: Int, l: List[T]): (List[T], T) = {
		assert(k>=0)
		assert(k<l.length)
		
		val (first, rest) = l.splitAt(k)
		(first ::: rest.drop(1), rest.head)
		
	}
	
	println("testing p20")
	val r1 = removeAt(1, List('a, 'b, 'c, 'd))
	assert(r1 == (List('a, 'c, 'd),'b), r1)
	val r2 = removeAt(3, List('a, 'b, 'c, 'd))
	assert(r2 == (List('a, 'b, 'c),'d), r2)	
}

/**
P21 (*) Insert an element at a given position into a list.
Example:
scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
 */
object p21 extends App {
	
	def insertAt[T](t: T, i: Int, l: List[T]): List[T] = {
		assert(i>0)
		assert(i<=l.length)
		
		val (first, rest) = l.splitAt(i)
		first ::: t :: rest
	}
	
	println("testing p21")
	val r1 = insertAt('new, 1, List('a, 'b, 'c, 'd))
	assert(r1 == List('a, 'new, 'b, 'c, 'd), r1)
	val r2 = insertAt('new, 4, List('a, 'b, 'c, 'd))
	assert(r2 == List('a, 'b, 'c, 'd, 'new), r2)
}


/**
P22 (*) Create a list containing all integers within a given range.
Example:
scala> range(4, 9)
res0: List[Int] = List(4, 5, 6, 7, 8, 9)
 */
object p22 extends App {
	
	def range(a: Int, b: Int): List[Int] = {
		assert(b >= a)
		if (a==b) a :: Nil else a :: range(a+1, b)
	}
	
	println("testing p22")
	val r1 = range(4, 9)
	assert(r1 == List(4, 5, 6, 7, 8, 9), r1)

}

/**
P23 (**) Extract a given number of randomly selected elements from a list.
Example:
scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
res0: List[Symbol] = List('e, 'd, 'a)
 */
object p23 extends App {	
	
	def randomSelect[A](a: Int, b: List[A]): List[A] = {
		import p20.removeAt
		assert(a >= 0)
		if (a == 0) Nil 
		else {
			val rand = new util.Random()
			val (rest, rem) = removeAt(rand.nextInt(b.length), b)
			rem :: randomSelect(a-1, rest)
		}
	}
	
	println("testing p23")
	val r1 = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
	println(s"randomly selected $r1")
	assert(r1.length == 3)
}


/**
P24 (*) Lotto: Draw N different random numbers from the set 1..M.
Example:
scala> lotto(6, 49)
res0: List[Int] = List(23, 1, 17, 33, 21, 37)
*/
object p24 extends App {
		
	def lotto(a: Int, b: Int): List[Int] = {
		import p23.randomSelect
		import p22.range
		randomSelect(a, range(0, b))
	}
	
	println("testing p24")
	val r1 = lotto(10, 100)
	println(s"lotto result is $r1")
	assert(r1.length == 10)
}


/**
P25 (*) Generate a random permutation of the elements of a list.
Example:
*/
object p25 extends App {
	
	def randomPermute[A](a: List[A]): List[A] = {
		import p23.randomSelect
		randomSelect(a.length, a)
	}
	
	println("testing p25")
	val in = List('a, 'b, 'c, 'd, 'e, 'f)
	val r1 = randomPermute(in)
	println(s"randompermute $in => $r1")
	assert(r1.length == in.length)
}

/**
P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) 
denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
Example:

Okay, fuck not using a set for this question.
*/
object p26 extends App {
	
	def combinations[T](a: Int, src: Set[T]): List[Set[T]] = 
		if (a > 0){
			src.flatMap(x => combinations(a-1, src - x).map(s => s + x)).toList
		} else List(Set())
	
	println(combinations(3, (1 to 12).toSet).size)
}


/**
 P27 (**) Group the elements of a set into disjoint subsets.
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
Example:

scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

Example:

scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).

You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
*/

object p27 extends App {
	//algorithmically awful.
	def group[T](a: List[Int], src: Set[T]): List[List[Set[T]]] = a match {
		case h :: t =>  for {
			g <- p26.combinations(h, src)
			r <- group(t, src -- g)
		} yield g :: r
		case Nil => List(Nil)
	}
	
	group(List(2, 2, 5), Set("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).foreach(println)
	
	
} 


/**
P28 (**) Sorting a list of lists according to length of sublists.
a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. 
E.g. short lists first, longer lists later, or vice versa.
Example:

scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements 
according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, 
others with a more frequent length come later.

Example:

scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. 
The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.
 */

object p28 extends App {
	
	def lsort[T](a: List[List[T]]): List[List[T]] = a.sortBy(_.length)
	def lsortFreq[T](a: List[List[T]]): List[List[T]] = {
		val r = a.groupBy(xs => xs.length)
		a.sortBy(x => r(x.length).length)
		
	}
	val r1 = lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
	val e1 = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
	assert(r1 == e1)
	
	val r2 =  lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
	val e2 = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
	assert(r2 == e2, r2)
	
	println("done testing 28")
}
