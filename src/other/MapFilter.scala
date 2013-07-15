package other

object MapFilter {

  def main(args: Array[String]): Unit = {
    val m0 = Map("abc" -> 1, "a" -> 3)
    val m1 = Map("ac" -> 123, "a" -> m0)
    val m2 = Map("d" -> m0, "aleph" -> m1)

    println(m2)

    val path = "aleph.a.abc"

    def go(path: List[String], m: Map[String, Any]): Map[String, Any] = path match {
      case x :: xs => {
        println(path)
        m.get(x) match {
          case Some(v: Map[String, Any]) => Map(x -> go(xs, v))
          case Some(v: Any)=> Map(x -> v)
          case None => throw new Exception("path not found")
        }
      }
      case Nil => m
    }

    println(go(path.split('.').toList, m2))

  }

}