package complete._0to99

object Problem2 {

	//sum of all even fibonacci sequence terms with value < 4 million
	def main(args: Array[String]): Unit = {
		val even_fibs = for {
			fib <- fib_until(4000000)
			if fib % 2 == 0
		} yield fib

		assert(even_fibs.sum == 4613732)
	}

	//compute fibonacci sequence until max
	def fib_until(max: Int) = {
		def go(a: Int, b: Int, acc: Set[Int] = Set.empty): Set[Int] = b match {
			case x if x > max => acc
			case x => go(b, a + b, acc + b)
		}

		go(1, 2)
	}
}