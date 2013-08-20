package complete._100to999

import math.BigInt
import scala.annotation.tailrec
import scala.math.BigInt.int2bigInt

/*
For any two strings of digits, A and B, we define FA,B to be the sequence (A,B,AB,BAB,ABBAB,...) in which each term is the concatenation of the previous two.

Further, we define DA,B(n) to be the nth digit in the first term of FA,B that contains at least n digits.

Example:

Let A=1415926535, B=8979323846. We wish to find DA,B(35), say.

The first few terms of FA,B are:
1415926535
8979323846
14159265358979323846
897932384614159265358979323846
14159265358979323846897932384614159265358979323846
Then DA,B(35) is the 35th digit in the fifth term, which is 9.

Now we use for A the first 100 digits of pi behind the decimal point:

14159265358979323846264338327950288419716939937510 
58209749445923078164062862089986280348253421170679

and for B the next hundred digits:

82148086513282306647093844609550582231725359408128 
48111745028410270193852110555964462294895493038196 .

Find sum{{ n = 0,1,...,17   10n× DA,B((127+19n)×7n) .
 */

object Problem230 {
  def main(args: Array[String]): Unit = {

    //HINT/READ THIS: http://en.wikipedia.org/wiki/L-System#Example_2:_Fibonacci_numbers
    
    val a = "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
    val b = "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"

    val res = (1 to 17) map ({ n: Int =>
      val j = (127 + 19 * n) * math.pow(7, n)
      fib_words(a, b, j toInt)
    })

    println(res.foldLeft("")((a: String, b: Char) => a + b))

    //println(fib_words("1415926535", "8979323846", 50))

  }

  def nth_digit(list: List[String], digits: BigInt, acc: BigInt = 0): Option[Char] = list match {
    case Nil => None
    case x :: xs => {
      val total = acc + x.length
      if (total >= digits) {
        val i: Int = (digits - acc - 1).toInt
        Some(x(i))
      } else {
        nth_digit(xs, digits, acc + x.length)
      }
    }

  }

  /*
   
   while working, summarize tail as BigInt-length and 
  
   */

  //first fibonacci sequence member that contains at least n digits
  def fib_words(a: String, b: String, digits: BigInt): Char = {
    println(s"D[$a],[$b]($digits)")

    nth_digit(List(a), digits) match {
      case Some(c) => return c
      case _ =>
    }

    nth_digit(List(b), digits) match {
      case Some(c) => return c
      case _ =>
    }

    //memoize dis
    @tailrec
    def go(a: List[String], b: List[String]): Char = nth_digit(b, digits) match {
      case Some(c) => c
      case None => go(b, a ::: b)
    }

    return go(List(a), List(b))
  }

}