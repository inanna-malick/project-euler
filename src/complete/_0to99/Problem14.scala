package complete._0to99
import scala.annotation.tailrec

object Problem14 {
	// Which starting number, under one million, produces the longest collatz sequence?
	def main(args: Array[String]): Unit = {
		
		@tailrec
		def collatz(n: Long, c: Int = 0): Int = if (n==1) c + 1 else collatz(if(n%2==0) n/2 else (3*n) + 1, c + 1)

		val re = (1 until 1000000).map{n => (n, collatz(n))}.reduceLeft((a, b) => if(a._2 > b._2) a else b)._1
		println(re)
	}
}