import scala.annotation.tailrec

object Day11 extends Day(11) {

  type SeatLayout = Array[Array[Char]]

  val FLOOR    = '.'
  val OCCUPIED = '#'
  val EMPTY    = 'L'

  def run1() = {
    val input = readLines.map(_.toCharArray).toArray
    countOccupied(getStabilizedLayout(input, skipFloor = false, clearCount = 4))
  }

  def run2() = {
    val input = readLines.map(_.toCharArray).toArray
    countOccupied(getStabilizedLayout(input, skipFloor = true, clearCount = 5))
  }

  def countOccupied(arr: SeatLayout): Int = arr.map(_.count(_ == OCCUPIED)).sum

  @tailrec
  def getStabilizedLayout(
      input: SeatLayout,
      skipFloor: Boolean,
      clearCount: Int
  ): SeatLayout = {

    val output = input.zipWithIndex.map {
      case (row, i) =>
        row.zipWithIndex.map {
          case (EMPTY, j) if getAdjacentOccupied(input, i, j, skipFloor) == 0 =>
            OCCUPIED
          case (OCCUPIED, j) if getAdjacentOccupied(input, i, j, skipFloor) >= clearCount =>
            EMPTY
          case (c, _) =>
            c
        }
    }

    if (layoutsAreEqual(input, output))
      output
    else
      getStabilizedLayout(output, skipFloor, clearCount)
  }

  def layoutsAreEqual(first: SeatLayout, second: SeatLayout): Boolean =
    first.zipWithIndex.forall { case (arr, i) => arr sameElements second(i) }

  def getAdjacentOccupied(input: SeatLayout, i: Int, j: Int, skipFloor: Boolean): Int =
    getAdjacent(input, i, j, skipFloor).count(_ == OCCUPIED)

  def getAdjacent(input: SeatLayout, i: Int, j: Int, skipFloor: Boolean): List[Char] =
    (for {
      x <- List(-1, 0, 1)
      y <- List(-1, 0, 1) if !(y == 0 && x == 0)
    } yield getAdjacentSeat(input, i, j, x, y, skipFloor)).flatten

  @tailrec
  def getAdjacentSeat(
      input: SeatLayout,
      i: Int,
      j: Int,
      iDiff: Int,
      jDiff: Int,
      skipFloor: Boolean
  ): Option[Char] =
    getSeatSafe(input, i + iDiff, j + jDiff) match {
      case Some(FLOOR) if skipFloor =>
        getAdjacentSeat(input, i + iDiff, j + jDiff, iDiff, jDiff, skipFloor)
      case seat =>
        seat
    }

  def getSeatSafe(input: SeatLayout, i: Int, j: Int): Option[Char] =
    if (i > input.length - 1 || i < 0 || j < 0 || j > input(0).length - 1)
      None
    else
      Some(input(i)(j))
}
