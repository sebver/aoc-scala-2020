import scala.annotation.tailrec

object Day15 extends Day(15) {

  val example = "0,3,6"

  def run1() = {
    val input = example.split(",").map(_.toLong)
    playGame1(input.length, input.last, input.zipWithIndex.dropRight(1).toMap, 2020)
  }

  @tailrec
  def playGame1(current: Int, currentValue: Long, lastIndex: Map[Long, Int], end: Int): Long = {
    if (current == end)
      currentValue
    else {
      lastIndex.get(currentValue) match {
        case None =>
          playGame1(current + 1, 0, lastIndex.updated(currentValue, current - 1), end)
        case Some(idx) =>
          playGame1(
            current + 1,
            current - idx - 1,
            lastIndex.updated(currentValue, current - 1),
            end
          )
      }
    }
  }
}
