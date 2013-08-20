package complete._0to99

object Problem11 {
	// What is the greatest product of four adjacent numbers in the same direction
	// (up, down, left, right, or diagonally) in the 20×20 grid?
	def main(args: Array[String]): Unit = {
		val data = io.Source.fromFile("data/q11data").getLines.map(line => line.split(" ").map(n => n.toInt)).toArray
		
		val height = data.length
		val width = data.head.length
		val len = 4
		
		//split the grid into all possible sub-grids of len x len
		val cubes = for {
			x <- (0 to width - len)
			y <- (0 to height - len)
		} yield data.view(y, y + len).map(row => row.view(x, x+4))
		
		val groups = {
			(for { //left, right
				cube <- cubes
				row <- cube
			} yield row) ++
			(for { //up, down
				cube <- cubes
				column <- (0 until len)
			} yield cube.map(row => row(column))) ++
			(for { //diagonally
				cube <- cubes
				diagonal <- Set((0 until len).map(n => cube(n)(n)),
								(0 until len).map(n => cube(len - n - 1)(n)))
			} yield diagonal)
		}		
		
		assert(groups.map(_.product).max == 70600674)
	}
	
}