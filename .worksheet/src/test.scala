object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(57); 
  println("Welcome to the Scala worksheet");$skip(29); 
  
  println(factors(13195));$skip(56); 
  
def divides(d : Int, n : Int) = {
   (n % d) == 0
 };System.out.println("""divides: (d: Int, n: Int)Boolean""");$skip(44); 

 def ld(n : Int) : Int = {
   ldf(2, n)
 };System.out.println("""ld: (n: Int)Int""");$skip(112); 

 def ldf(k : Int, n : Int) : Int = {
   if (divides(k, n)) k
   else if ((k*k) > n) n
   else ldf((k+1), n)
 };System.out.println("""ldf: (k: Int, n: Int)Int""");$skip(132); 

 def factors(n : Int) : List[Int] = n match {
   case 1 => Nil;
   case _ => {
     val p = ld(n)
     p :: factors(n / p)
   }
 };System.out.println("""factors: (n: Int)List[Int]""");$skip(36); 

	def adder(m: Int, n: Int) = m + n;System.out.println("""adder: (m: Int, n: Int)Int""");$skip(37); val res$0 = 

 
 
	(1 :: 2 :: Nil).fold(0)(_ + _);System.out.println("""res0: Int = """ + $show(res$0));$skip(77); val res$1 = 
	
	
	for  {
		x <- List("a", "b")
		y <- List("c", "d")
		s = x+y
	} yield s;System.out.println("""res1: List[String] = """ + $show(res$1))}
                                                  


}
