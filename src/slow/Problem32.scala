
/*
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39  186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
 */

package slow

object Problem32 {
  def main(args: Array[String]): Unit = {

    
    //TODO: combinations is painfully slow, find smarter way to explore space
    
    val pandigital_pairs = (1 until 2000).combinations(2).filter({ case Seq(a, b) => println(a, b, a * b); pandigital(List(a, b, a * b)) })

    println(pandigital_pairs.foldLeft(0)((x, y) => y match { case Seq(a, b) => a * b }))

  }

  def pandigital(n: List[Int]): Boolean = {
    val d = n.flatMap(digits(_))
    (1 until 10).forall(d contains _)
  }

  def digits(i: Int): List[Int] = i match {
    case 0 => Nil
    case x => i % 10 :: digits(i / 10)

  }
}