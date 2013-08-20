package complete._0to99

object Problem6 {
	//Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
	def main(args: Array[String]): Unit = {
		val sqs = for{
			x <- (1 to 100)
		} yield x * x
		val sum_of_sq = sqs.sum
		
		val sum = (1 to 100).sum
		val sq_of_sum = sum * sum

		assert(sq_of_sum - sum_of_sq == 25164150)
	}

}