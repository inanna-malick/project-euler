/*
 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

 What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 */

package complete

object Problem5 {
  def main(args: Array[String]): Unit = {
    println((1 to 20).foldLeft(1)((a, b) => if (a % b == 0) a else lcm(a, b)))
  }

  def gcd(a: Int, b: Int): Int = b match {
    case 0 => a.abs
    case x => gcd(b, a % b)
  }

  def lcm(a: Int, b: Int): Int = (a * b).abs / gcd(a, b)

}