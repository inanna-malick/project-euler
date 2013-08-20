package complete._0to99

object Problem13 {
	// Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
	def main(args: Array[String]): Unit = {
		val sum = io.Source.fromFile("data/q13data").getLines.map(n => n.take(11).toLong).sum.toString.take(10)
				
		println(sum)
	}
	
}