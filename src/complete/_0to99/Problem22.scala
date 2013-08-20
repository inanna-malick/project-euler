package complete._0to99

import scala.Array.canBuildFrom

/*
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, 
begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938  53 = 49714.

What is the total of all the name scores in the file?
 */

object Problem22 {

  def main(args: Array[String]): Unit = {

    def alphabet = "abcdefghijklmnopqrstuvwxyz".toCharArray()
    def alpha_value: Map[Char, Int] = (0 until alphabet.length).map(i => (alphabet(i), i + 1)) toMap

    def alpha_score(s: String): Int = s.toLowerCase().foldLeft(0)((a: Int, b: Char) => a + alpha_value(b))

    val names: List[String] = io.Source.fromFile("resources/p22-names.txt").getLines.flatMap(x => x.split(",").map(_.replace("\"", ""))).toList.sorted

    
    println((0 until names.length).foldLeft(0) { (acc: Int, i: Int) => acc + (i+1) * alpha_score(names(i)) })
  }

}