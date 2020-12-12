import scala.io.Source

object Day1 {

  val lines = Source.fromResource("day1.txt").getLines()

  val list = LazyList.from(lines.map(_.toInt))

  val results = for {
    (l1, i1) <- list.zipWithIndex
    (l2, i2) <- list.zipWithIndex
    (l3, i3) <- list.zipWithIndex
    if Set(i1, i2, i3).size == 3 && l1 + l2 + l3 == 2020
  } yield l1 * l2 * l3

  println(results.head)
}
