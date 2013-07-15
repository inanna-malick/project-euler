/*
The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*/

package complete

object Problem6 {
  def main(args: Array[String]): Unit = {
    val sum_of_sq = (1 to 100).foldLeft(0)((a, b) => a + b * b)
    val sum = (1 to 100).foldLeft(0)((a, b) => a + b)
    val sq_of_sum = sum * sum
    
    println(sq_of_sum - sum_of_sq)
  }

}