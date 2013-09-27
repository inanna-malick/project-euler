package data_structures

class Trie(val children: Map[Char, Trie] = Map(), val isEndPoint: Boolean = false) {

	def contains(a: List[Char]): Boolean = a match {
		case h :: t => children.contains(h) && children(h).contains(t)
		case Nil => isEndPoint
	}

	def add(a: List[Char]): Trie = a match {
		case h :: t => new Trie(children.updated(h, children.getOrElse(h, new Trie()).add(t)), isEndPoint)
		case Nil => new Trie(children, true)
	}
}

object main extends App {
	
	val t = new Trie()
	
	
}