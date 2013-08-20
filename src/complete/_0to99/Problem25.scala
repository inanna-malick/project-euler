
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

package complete._0to99

import math.BigInt
import scala.math.BigInt.int2bigInt

object Problem25 {
  def main(args: Array[String]): Unit = {

    println(fib_with(i => digits(i) == 1000))

  }

  //digits in Bigint
  def digits(i: BigInt): BigInt = i match {
    case x if x == 0 => 0
    case x => digits(i / 10) + 1
  }

  //sequence number of first fibonacci sequence member where f is true 
  def fib_with(f: BigInt => Boolean): BigInt = {
    if (f(1)) 1
    else if (f(2)) 3
    else {
      def go(a: BigInt, b: BigInt, count: Int): BigInt = b match {
        case x if f(x) => count
        case x => go(b, a + b, count + 1)
      }

      go(1, 2, 3)
    }
  }
}