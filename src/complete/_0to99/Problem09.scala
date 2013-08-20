package complete._0to99

object Problem9 {
	// A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^2 + b^2 = c^2
	// There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
	def main(args: Array[String]): Unit = {
			
		val results = for {
			c <- (1 to 999)
			b <- (1 to c)
			a <- (1 to b)
			if (a*a + b*b) == (c*c) && (a + b + c) == 1000
		} yield a*b*c
		
		assert(results.length == 1)
		assert(results.head == 31875000)
	}
}