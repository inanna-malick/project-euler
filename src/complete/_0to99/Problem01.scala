package complete._0to99

object Problem1 {
	//sum of all the multiples of 3 or 5 below 1000.
	def main(args: Array[String]): Unit = {
		val multiples = for {
			possible <- (1 to 999)
			if possible % 5 == 0 || possible % 3 == 0
		} yield possible

		assert(multiples.sum == 233168)
	}
}