/*
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
 */

package needs_refactor

import java.math.BigInteger

object Problem10 {

  def main(args: Array[String]): Unit = {
    println(sieveOfEratosthenes(2000000).foldLeft(new BigInteger("0"))((a, b) => a.add(BigInteger.valueOf(b))))
  }

  def ints(n: Int): Stream[Int] = Stream.cons(n, ints(n + 1)) 
  def sieve(nums: Stream[Int]): Stream[Int] =
    Stream.cons(nums.head, sieve((nums.tail) filter (_ % nums.head != 0))) 
  def primes = sieve(ints(2))

  /*
   * thank you, wikipedia
  Input: an integer n > 1
	Let A be an array of Boolean values, indexed by integers 2 to n,
	initially all set to true.
	 
	for i = 2, 3, 4, ..., vn :
	  if A[i] is true:
	    for j = i2, i2+i, i2+2i, ..., n:
	      A[j] := false
	 
	Now all i such that A[i] is true are prime.
   */
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