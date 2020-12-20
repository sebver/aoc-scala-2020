import scala.annotation.tailrec
import scala.util.{Success, Try}

object Day13 extends Day(13) {

  val example = """939
                  |7,13,x,x,59,x,31,19""".stripMargin.split("\n").toList

  def run1() = {
    val lines                = readLines
    val time                 = lines(0).toLong
    val ids                  = lines(1).split(",").filter(_ != "x").map(_.toLong)
    val (actualStart, busId) = find(time, ids)
    (actualStart - time) * busId
  }

  @tailrec
  def find(start: Long, busIds: Array[Long]): (Long, Long) = {
    busIds.find(start % _ == 0) match {
      case Some(busId) => (start, busId)
      case None        => find(start + 1, busIds)
    }
  }

  def run2() = {
    val inputWithIndex =
      readLines(1)
        .split(",")
        .zipWithIndex
        .filter(_._1 != "x")
        .map { case (str, i) => (str.toLong, i.toLong) }
        .toList
    val n = inputWithIndex.map(_._1)
    val a = inputWithIndex.map { case (n, a) => n - a }
    (n, a, chineseRemainder(n, a))
  }

  def chineseRemainder(n: List[Long], a: List[Long]): Option[Long] = {
    require(n.size == a.size)
    val prod = n.product

    def iter(n: List[Long], a: List[Long], sm: Long): Long = {
      def mulInv(a: Long, b: Long): Long = {
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }
}
