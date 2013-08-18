package complete

object Problem7 {
	//What is the 10 001st prime number?
	def main(args: Array[String]): Unit = {
		assert(primes_until(10001).last == 104743)
	}

	def primes_until(until: Int): Seq[Int] = {
		@scala.annotation.tailrec
		def loop(i: Int, primes_found: Seq[Int]): Seq[Int] = {
			if (primes_found.length >= until) 
				primes_found
			else {
				if (primes_found exists (x => i % x == 0)) 
					loop(i + 2, primes_found) // a prime exists that evenly divides this, continue
				else loop(i + 2, primes_found :+ i)	// a new prime is found
			}
		}
		if (until <= 1) Seq(2)
		else loop(3, Seq(2))
	}

}