/*
Starting in the top left corner of a 22 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 2020 grid?
 */

package complete

object Problem15 {

  def main(args: Array[String]): Unit = {

    ((grid_size - 1) to 1).foreach(x => routes(x, x))

    println(routes(0, 0))
  }

  val grid_size = 21

  var cache: Array[Array[Long]] = Array.ofDim[Long](grid_size, grid_size)

  //x by x grid
  def routes(x: Int, y: Int): Long = {
    if (x >= grid_size || y >= grid_size) 0
    else if (cache(x)(y) != 0) cache(x)(y)
    else if (x == grid_size - 1 && y == grid_size - 1) 1
    else {
      val r = routes(x + 1, y) + routes(x, y + 1)
      cache(x)(y) = r
      r
    }
  }

}