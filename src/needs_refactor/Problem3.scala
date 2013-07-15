package needs_refactor

/*
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
 */

object Problem3 {

  def main(args: Array[String]): Unit = {
    println(factors(600851475143L).last)
  }

  def ld(n: Long): Long = {
    ldf(2, n)
  } //> ld: (n: Int)Int

  def ldf(k: Long, n: Long): Long = {
    if (n % k == 0) k
    else if ((k * k) > n) n
    else ldf((k + 1), n)
  } //> ldf: (k: Int, n: Int)Int

  def factors(n: Long): List[Long] = n match {
    case 1 => Nil;
    case _ => {
      val p = ld(n)
      p :: factors(n / p)
    }
  } //> factors: (n: Int)List[Int]

}