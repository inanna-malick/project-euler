package complete._0to99

object Problem3 {
	
	//What is the largest prime factor of the number 600851475143?
	def main(args: Array[String]): Unit = {
		assert(factors(600851475143L).last == 6857)
	}

	def ldf(k: Long, n: Long): Long = {
		if (n % k == 0) k
		else if ((k * k) > n) n
		else ldf((k + 1), n)
	}

	def factors(n: Long): List[Long] = n match {
		case 1 => Nil;
		case _ => {
			val p = ldf(2, n)
			p :: factors(n / p)
		}
	}

}