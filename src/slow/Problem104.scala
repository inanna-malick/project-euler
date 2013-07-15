
/*
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144
The 12th term, F12, is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 1000 digits?
 */

package slow

import math.BigInt
import scala.annotation.tailrec
import scala.math.BigInt.int2bigInt

object Problem104 {
  def main(args: Array[String]): Unit = {

    def pandigital_post(i: BigInt) = pandigital(digits(i, Nil).reverse.take(9))
    def pandigital_pre(i: BigInt) = pandigital(digits(i, Nil).take(9))

    println(fib_with(i => pandigital_post(i) && pandigital_pre(i)))

  }

  //digits in Bigint
  @tailrec
  def digits(i: BigInt, acc: List[Int]): List[Int] = i match {
    case x if x == 0 => acc
    case x => digits(i / 10, (i % 10).toInt :: acc)
  }

  def pandigital(n: List[Int]): Boolean = {
    val d = n.flatMap(digits(_, Nil))
    (1 until 10).forall(d contains _)
  }

  @tailrec
  def digits(i: Int, acc: List[Int]): List[Int] = i match {
    case 0 => acc
    case x => digits(i / 10, (i % 10) :: acc)
  }

  //sequence number of first fibonacci sequence member where f is true 
  def fib_with(f: BigInt => Boolean): BigInt = {
    if (f(1)) 1
    else if (f(2)) 3
    else {
      @tailrec
      def go(a: BigInt, b: BigInt, count: Int): BigInt = b match {
        case x if f(x) => count
        case x => go(b, a + b, count + 1)
      }

      go(1, 2, 3)
    }
  }
}