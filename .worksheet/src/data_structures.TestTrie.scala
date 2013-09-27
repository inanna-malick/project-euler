package data_structures

object TestTrie {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(86); 
  println("Welcome to the Scala worksheet");$skip(81); 
   
  
  val words = List("hello", "world", "test", "is", "this", "thing", "on");System.out.println("""words  : List[String] = """ + $show(words ));$skip(74); 
  val t = words.foldLeft(new Trie())((acc, word) => acc.add(word.toList));System.out.println("""t  : data_structures.Trie = """ + $show(t ));$skip(73); val res$0 = 
	(words :+ "foobar" :+ "hell").map(s => s.toList).toList.map(t.contains);System.out.println("""res0: List[Boolean] = """ + $show(res$0))}
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
