package ninetynineproblems
import IntImplicits._

object Ints {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  (3 to 20).map(i => (i, i.isPrime))              //> res0: scala.collection.immutable.IndexedSeq[(Int, Boolean)] = Vector((3,true
                                                  //| ), (4,false), (5,true), (6,false), (7,true), (8,false), (9,false), (10,false
                                                  //| ), (11,true), (12,false), (13,true), (14,false), (15,false), (16,false), (17
                                                  //| ,true), (18,false), (19,true), (20,false))
}



case class IntWrapper(value: Int){
	def isPrime: Boolean = !(2 to (value/2)).exists(x => value % x == 0)
}

object IntImplicits {
   implicit def Int2IntWrap(value : Int): IntWrapper = IntWrapper(value)
}