package algorithms_class.greedy

/**
If a graph is empty then we are done immediately. Thus, we assume otherwise.
The algorithm starts with a tree consisting of a single vertex, and continuously increases its size one edge at a time, until it spans all vertices.
Input: A non-empty connected weighted graph with vertices V and edges E (the weights can be negative).
Initialize: Vnew = {x}, where x is an arbitrary node (starting point) from V, Enew = {}
Repeat until Vnew = V:
Choose an edge {u, v} with minimal weight such that u is in Vnew and v is not (if there are multiple edges with the same weight, any of them may be picked)
Add v to Vnew, and {u, v} to Enew
Output: Vnew and Enew describe a minimal spanning tree
 */

//WRONG(?) at least according to https://class.coursera.org/algo2-002/forum/thread?thread_id=33
object PrimMST extends App{
	import scala.collection.mutable.Map
	type Edge = (Int, Int, Int) //node 1, node 2, edge cost
	type Graph = Map[Int, List[Edge]] //n1 is always This node
	
	val edges: List[Edge] = 
		//parse(io.Source.fromFile("C:/git/project-euler/src/algorithms_class/greedy/ex3").getLines.toList)
		parse(io.Source.fromURL("http://spark-public.s3.amazonaws.com/algo2/datasets/edges.txt").getLines.toList)
		
		
	def parse(src: List[String]): List[Edge] = 
		src.drop(1).map(_.split(" ")).map{case Array(n1, n2, cost) => (n1.toInt, n2.toInt, cost.toInt)}
	
	def adjacencyList(edges: List[Edge], result: Graph = Map().withDefaultValue(Nil)): Graph = edges match {
		case (n1, n2, cost) :: tail => {
			result.update(n1, (n1, n2, cost) :: result(n1))
			result.update(n2, (n2, n1, cost) :: result(n2))
			adjacencyList(tail, result)
		}
		case Nil => result
	}
	
	
	def prim(g: Graph): List[Edge] = {		
		def next(vNew: Set[Int]): Edge = {for {
				key <- g.keys
				if vNew.contains(key)
				(n1, n2, cost) <- g(key)
				if !vNew.contains(n2)
			} yield {assert(key==n1); (n1, n2, cost)} }.minBy{case (_, _, cost) => cost}
		
		def go(vNew: Set[Int], eNew: Set[Edge]): Set[Edge] =
			if (g.keys.size == vNew.size) eNew
			else {
				val r = next(vNew)
				val (n1, n2, cost) = r
				go(vNew + n2, eNew + r)
			}
		
		go(Set(g.keys.head), Set()).toList
	}
	
	val result = prim(adjacencyList(edges))
	println(result.map{case (_, _, cost) => cost.toLong}.sum)
}