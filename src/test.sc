object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  println(factors(13195))                         //> List(5, 7, 13, 29)
  
def divides(d : Int, n : Int) = {
   (n % d) == 0
 }                                                //> divides: (d: Int, n: Int)Boolean

 def ld(n : Int) : Int = {
   ldf(2, n)
 }                                                //> ld: (n: Int)Int

 def ldf(k : Int, n : Int) : Int = {
   if (divides(k, n)) k
   else if ((k*k) > n) n
   else ldf((k+1), n)
 }                                                //> ldf: (k: Int, n: Int)Int

 def factors(n : Int) : List[Int] = n match {
   case 1 => Nil;
   case _ => {
     val p = ld(n)
     p :: factors(n / p)
   }
 }                                                //> factors: (n: Int)List[Int]

	def adder(m: Int, n: Int) = m + n         //> adder: (m: Int, n: Int)Int

 
 
	(1 :: 2 :: Nil).fold(0)(_ + _)            //> res0: Int = 3
	
	
	for  {
		x <- List("a", "b")
		y <- List("c", "d")
		s = x+y
	} yield s                                 //> res1: List[String] = List(ac, ad, bc, bd)
                                                  


}