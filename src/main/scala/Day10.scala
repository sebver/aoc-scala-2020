import scala.annotation.tailrec
import scala.collection.mutable

object Day10 extends Day(10) {

  def run1() = {
    val input = readLines.map(_.toLong).toSet

    val (diff1, diff3) = countDiffs(input, 0)

    diff1 * diff3
  }

  @tailrec
  def countDiffs(
      adapterSet: Set[Long],
      current: Long,
      diff1: Long = 0,
      diff3: Long = 0
  ): (Long, Long) = {
    List(1, 2, 3).find(i => adapterSet.contains(current + i)) match {
      case Some(i) if i == 1 =>
        countDiffs(adapterSet, current + i, diff1 + 1, diff3)
      case Some(i) if i == 2 =>
        countDiffs(adapterSet, current + i, diff1, diff3)
      case Some(i) if i == 3 =>
        countDiffs(adapterSet, current + i, diff1, diff3 + 1)
      case _ =>
        (diff1, diff3 + 1)
    }
  }

  def run2() = countArrangements(readLines.map(_.toLong).toSet)

  def countArrangements(
      adapterSet: Set[Long],
      current: Long = 0,
      cache: mutable.HashMap[Long, Long] = mutable.HashMap.empty
  ): Long =
    cache.getOrElse(
      current, {
        val count = List(1, 2, 3).filter(i => adapterSet.contains(current + i)) match {
          case Nil =>
            1
          case possibleNext =>
            possibleNext
              .map(i => countArrangements(adapterSet, current + i, cache))
              .sum
        }

        cache.addOne((current, count))

        count
      }
    )
}
