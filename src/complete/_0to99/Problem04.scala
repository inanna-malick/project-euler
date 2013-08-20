package complete

object Problem4 {
	//Find the largest palindrome made from the product of two 3-digit numbers.
	def main(args: Array[String]): Unit = {
		val palindromes = for {
			a <- (100 to 999)
			b <- (100 to a)
			if isPalindrome(a * b)
		} yield a * b

		assert(palindromes.max == 906609)
	}

	def digits(a: Int, acc: Vector[Int] = Vector.empty): Vector[Int] = {
		if (a > 0) digits(a / 10, acc :+ a % 10)
		else acc
	}

	def isPalindrome(a: Int): Boolean = {
		val d = digits(a)
		(0 to d.length / 2).forall(idx => (d(idx) == d(d.length - 1 - idx)))
	}

}