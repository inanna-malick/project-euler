package complete._0to99

object Problem5 {
	//What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
	def main(args: Array[String]): Unit = {
		assert((1 to 20).foldLeft(1)((a, b) => if (a % b == 0) a else lcm(a, b)) == 232792560)
	}

	def gcd(a: Int, b: Int): Int = b match {
		case 0 => a.abs
		case x => gcd(b, a % b)
	}

	def lcm(a: Int, b: Int): Int = (a * b).abs / gcd(a, b)

}