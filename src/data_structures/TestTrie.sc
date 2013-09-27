package data_structures

object TestTrie {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
   
  
  val words = List("hello", "world", "test", "is", "this", "thing", "on")
                                                  //> words  : List[String] = List(hello, world, test, is, this, thing, on)
  val t = words.foldLeft(new Trie())((acc, word) => acc.add(word.toList))
                                                  //> t  : data_structures.Trie = data_structures.Trie@ad8bfd
	(words :+ "foobar" :+ "hell").map(s => s.toList).toList.map(t.contains)
                                                  //> res0: List[Boolean] = List(true, true, true, true, true, true, true, false, 
                                                  //| false)
}

class Trie2(children: Map[Char, Trie] = Map(), isEndPoint: Boolean = false) {

	def contains(a: List[Char]): Boolean = a match {
		case h :: t => children.contains(h) && children(h).contains(t)
		case Nil => isEndPoint
	}

	def add(a: List[Char]): Trie = a match {
		case h :: t => new Trie(children.updated(h, children.getOrElse(h, new Trie()).add(t)), isEndPoint)
		case Nil => new Trie(children, true)
	}
		
}