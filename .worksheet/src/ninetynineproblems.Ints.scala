package ninetynineproblems
import IntImplicits._

object Ints {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(107); 
  println("Welcome to the Scala worksheet");$skip(40); val res$0 = 
  
  (3 to 20).map(i => (i, i.isPrime));System.out.println("""res0: scala.collection.immutable.IndexedSeq[(Int, Boolean)] = """ + $show(res$0))}
}



case class IntWrapper(value: Int){
	def isPrime: Boolean = !(2 to (value/2)).exists(x => value % x == 0)
}

object IntImplicits {
   implicit def Int2IntWrap(value : Int): IntWrapper = IntWrapper(value)
}
