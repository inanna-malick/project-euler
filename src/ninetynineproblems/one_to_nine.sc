//http://aperiodic.net/phil/scala/s-99/
object _99_Scala_Problems {

	def last[T](a: List[T]): T = a match {
		case h :: Nil => h
		case h :: t => last(t)
		case _ => throw new Exception
	}                                         //> last: [T](a: List[T])T

	last(List(1, 2, 3))                       //> res0: Int = 3
	last(List('a'))                           //> res1: Char = a

	def penultimate[T](a: List[T]): T = a match {
		case a :: b :: Nil => a
		case h :: t => penultimate(t)
		case _ => throw new Exception
	}                                         //> penultimate: [T](a: List[T])T

	penultimate(List(1, 1, 2, 3, 5, 8))       //> res2: Int = 5

	def nth[T](n: Int, a: List[T]): T = a match {
		case h :: t => if (n <= 0) h else nth(n - 1, t)
		case _ => throw new Exception
	}                                         //> nth: [T](n: Int, a: List[T])T

	nth(2, List(1, 1, 2, 3, 5, 8))            //> res3: Int = 2

	def length[T](a: List[T], acc: Int = 0): Int = a match {
		case h :: t => length(t, acc + 1)
		case _ => acc
	}                                         //> length: [T](a: List[T], acc: Int)Int

	length(List(1, 1, 2, 3, 5, 8))            //> res4: Int = 6

	def reverse[T](a: List[T], acc: List[T] = Nil): List[T] = a match {
		case h :: t => reverse(t, h :: acc)
		case _ => acc
	}                                         //> reverse: [T](a: List[T], acc: List[T])List[T]

	reverse(List(1, 1, 2, 3, 5, 8))           //> res5: List[Int] = List(8, 5, 3, 2, 1, 1)

	def isPalindrome[T](a: List[T]): Boolean = {
		val reversed = reverse(a)
		a == reversed
	}                                         //> isPalindrome: [T](a: List[T])Boolean

	isPalindrome(List(1, 2, 3, 2, 1))         //> res6: Boolean = true

	def flatten(ls: List[Any]): List[Any] = ls flatMap {
		case ms: List[_] => flatten(ms)
		case e => List(e)
	}                                         //> flatten: (ls: List[Any])List[Any]

	flatten(List(List(1, 1), 2, List(3, List(5, List(8)))))
                                                  //> res7: List[Any] = List(1, 1, 2, 3, 5, 8)

	//scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	//res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

	def compress[Symbol](a: List[Symbol]): List[Symbol] = {

		def go(a: List[Symbol], prev: Symbol, acc: List[Symbol]): List[Symbol] = a match {
			case h :: t if h == prev => go(t, prev, acc)
			case h :: t => go(t, h, h :: acc)
			case _ => reverse(acc)
		}

		a match {
			case Nil => Nil
			case h :: t => go(a, h, h :: Nil)
		}

	}                                         //> compress: [Symbol](a: List[Symbol])List[Symbol]

	compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e, 'f))
                                                  //> res8: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e, 'f)

	/*P09 (**) Pack consecutive duplicates of list elements into sublists.
	scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
	*/

	def pack(a: List[Symbol]): List[List[Symbol]] = {
		if (!a.isEmpty) {
			val (packed, next) = a.span(_ == a.head)
			if (next == Nil) List(packed)
			else packed :: pack(next)
		} else
			List(List())
	}                                         //> pack: (a: List[Symbol])List[List[Symbol]]

	pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res9: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c
                                                  //| ), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
	pack(List())                              //> res10: List[List[Symbol]] = List(List())

}