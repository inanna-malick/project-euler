/*
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
 */

package complete._0to99

object Problem10 {

	def main(args: Array[String]): Unit = {
		println(sieveOfEratosthenes(2000000).foldLeft(BigInt(0))((a, b) => a + BigInt(b)))

	}

	def sieveOfEratosthenes(n: Int) = {
		val primes = new scala.collection.mutable.BitSet(n)
		primes ++= (2 to n)
		val sqrt = Math.sqrt(n).toInt
		for {
			candidate <- 2 to sqrt
			if primes contains candidate
		} primes --= candidate * candidate to n by candidate
		primes
	}

}