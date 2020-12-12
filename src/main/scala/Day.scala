import scala.io.Source

abstract class Day(day: Int) {

  def readLines: List[String] = Source.fromResource(s"day$day.txt").getLines().toList

  def splitAtBlanks(input: List[String]): Array[Array[String]] =
    input
      .mkString("\n")
      .split("\\n\\n")
      .map(_.split("\\n"))
}
