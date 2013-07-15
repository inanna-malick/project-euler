package complete


object Problem7 {

  def main(args: Array[String]): Unit = {
    println(primes_until(10001).last)
  }
  
  def primes_until(until: Int): Seq[Int] = {
    @scala.annotation.tailrec
    def loop(i: Int, primes: Seq[Int]): Seq[Int] = {
      if (primes.length >= until) primes // we reached the desired end
      else {
        if (primes exists (x => i % x == 0)) loop(i + 2, primes) // we already found a factor of this i
        else loop(i + 2, primes :+ i)
      }
    }
    if (until <= 1) Seq(2)
    else loop(3, Seq(2))
  }

}