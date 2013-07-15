
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

package complete

import math.BigInt
import scala.annotation.tailrec
import scala.math.BigInt.int2bigInt

object Problem104 {
  def main(args: Array[String]): Unit = {

    println(fib_with((idx, i) => idx > 2749 && pandigital((i % 1000000000).toInt) && pandigital(firstDigitsOfFib(idx))))

  }

  //evil voodoo, etc. credit: http://blog.singhanuvrat.com/problems/get-first-few-digits-of-a-fibonacci-number
  //  For the fibonacci number Fib(n), return the first d digits
  def firstDigitsOfFib(n: Int, d: Int = 9): Int = {
    val temp = n * 0.20898764024997873 - 0.3494850021680094
    math.pow(10, temp - temp.toInt + d - 1).toInt
  }

  //does int contain digits 1-9 
  @tailrec
  def pandigital(i: Int, rem: Set[Int] = Set(1, 2, 3, 4, 5, 6, 7, 8, 9)): Boolean = {
    if (rem.size == 0) true else (i % 10).toInt match {
      case x if rem.contains(x) => pandigital(i / 10, rem - x)
      case x => false
    }
  }

  //sequence number of first fibonacci sequence member where f is true 
  def fib_with(f: (Int, BigInt) => Boolean): Int = {
    if (f(1, 1)) 1
    else if (f(2, 1)) 2
    else {
      @tailrec
      def go(a: BigInt, b: BigInt, idx: Int): Int = b match {
        case x if f(idx, b) => idx
        case x => go(b, a + b, idx + 1)
      }

      go(1, 2, 3)
    }
  }
}