import scala.annotation.tailrec

object Day17 extends Day(17) {

  val example = """.#.
                  |..#
                  |###""".stripMargin.split("\n").toList

  def run1() = {
    val input = parse(readLines)
    runCycle(input, false, 6).size
  }
  def run2() = {
    val input = parse(readLines)
    runCycle(input, true, 6).size
  }

  @tailrec
  def runCycle(input: State, checkW: Boolean, cyclesLeft: Int): State = {
    if (cyclesLeft == 0)
      input
    else
      runCycle(
        State(
          (for {
            x <- (input.minX - 1) to (input.maxX + 1)
            y <- (input.minY - 1) to (input.maxY + 1)
            z <- (input.minZ - 1) to (input.maxZ + 1)
            w <- if (checkW) (input.minW - 1) to (input.maxW + 1) else 0 to 0
          } yield (
            input.contains((x, y, z, w)),
            input.getActiveNeighbours((x, y, z, w)).length
          ) match {
            case (true, 2 | 3) =>
              Some((x, y, z, w))
            case (false, 3) =>
              Some((x, y, z, w))
            case _ =>
              None
          }).flatten.toArray
        ),
        checkW,
        cyclesLeft - 1
      )
  }

  def parse(lines: List[String]) =
    State(
      lines
        .map(_.zipWithIndex.filter(_._1 == '#').map(_._2).toArray)
        .zipWithIndex
        .flatMap {
          case (row, i) => row.map((i, _, 0, 0))
        }
        .toArray
    )

  type Point = (Int, Int, Int, Int)

  case class State(actives: Array[Point]) {

    def size = actives.length

    def contains(point: Point) = actives.contains(point)

    def minX = actives.map(_._1).min
    def maxX = actives.map(_._1).max
    def minY = actives.map(_._2).min
    def maxY = actives.map(_._2).max
    def minZ = actives.map(_._3).min
    def maxZ = actives.map(_._3).max
    def minW = actives.map(_._4).min
    def maxW = actives.map(_._4).max

    def getActiveNeighbours(point: Point) = actives.filter {
      a =>
        Math.abs(a._1 - point._1) <= 1 &&
        Math.abs(a._2 - point._2) <= 1 &&
        Math.abs(a._3 - point._3) <= 1 &&
        Math.abs(a._4 - point._4) <= 1 &&
        !(a._1 == point._1 && a._2 == point._2 && a._3 == point._3 && a._4 == point._4)
    }
  }
}
