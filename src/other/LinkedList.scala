package other

import scala.collection.mutable.LinkedList
import scala.annotation.tailrec

object Linked {

  def main(args: Array[String]): Unit = {
    val l1 = LinkedList(1, 2, 3)

    println(l1)

    def reverse(list: LinkedList[Int]): LinkedList[Int] = list.next match {
      case LinkedList() => list
      case x => {
        val r = reverse(x)
        x.next = list
        list.next = LinkedList()
        r
      }

    }

    @tailrec
    def reverse_tailrec(list: LinkedList[Int], prev: LinkedList[Int] = LinkedList()): LinkedList[Int] = list.next match {
      case LinkedList() => {
        list.next = prev
        list
      }
      case x => {
        list.next = prev
        prev.next = LinkedList()
        reverse_tailrec(x, list)
      }

    }

    println(reverse_tailrec(l1))

  }

}