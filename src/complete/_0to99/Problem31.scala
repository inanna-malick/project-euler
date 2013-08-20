package complete._0to99

/*
In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1£1 + 150p + 220p + 15p + 12p + 31p
How many different ways can £2 be made using any number of coins?
 */

object Problem31 {

  def main(args: Array[String]): Unit = {

    val max = 200
    
    def go(denom: List[Int], acc: Int): Int = acc match {
      case 200 => 1
      case x if (x > 200) => 0
      case x => denom.tail match {
        case Nil => go(denom, acc + denom.head)
        case y => go(denom.tail, acc) + go(denom, acc + denom.head)
      }
    }

    println(go(denominations.reverse, 0))

  }

  val denominations = List(1, 2, 5, 10, 20, 50, 100, 200)

}