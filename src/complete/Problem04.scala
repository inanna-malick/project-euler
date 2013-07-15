
/*
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.

Find the largest palindrome made from the product of two 3-digit numbers.
 */

package complete

object Problem4 {
  def main(args: Array[String]): Unit = {
    //every palindrome resulting from a*b where a != b
    val combo_results = (100 to 999).combinations(2).map( x => x.foldLeft(1)( (a,b) => a*b) ).filter(isPalindrome)
    //every palindrome resulting from a*b where a == b
    val square_results = (100 to 999).map( x => x*x ).filter(isPalindrome)
    
    println((combo_results ++ square_results).max)
  }
  
  //loop through string version of a by index
  def isPalindrome(a: Int): Boolean = {
    val s = a.toString
    (0 to s.length/2).foldLeft(true)((a, b) => a && (s(b) == s(s.length - 1 - b)))
  }
  
}