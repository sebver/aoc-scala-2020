object Day9 extends Day(9) {

  def run1() = {

    val numbers = readLines.map(_.toLong).toArray

    val preamble = 25

    val input = numbers.sliding(preamble).toArray.dropRight(1).zipWithIndex.map {
      case (slice, index) => (slice, numbers(index + preamble))
    }

    input.collectFirst { case (slice, sum) if !verifySum(slice, sum) => sum }
  }

  def verifySum(part: Array[Long], sum: Long): Boolean = {
    for (combinationSum <- part.combinations(2).map(_.sum).toList.sorted)
      if (combinationSum > sum)
        return false
      else if (combinationSum == sum)
        return true

    false
  }

  def run2() = {

    val numbers = readLines.map(_.toLong).toArray

    getSliceWithSum(numbers, run1().get, 2 to 40)
  }

  def getSliceWithSum(numbers: Array[Long], sum: Long, range: Range): Option[Long] =
    LazyList
      .from(range)
      .flatMap(size => LazyList.from(numbers.sliding(size)))
      .collectFirst {
        case arr if arr.sum == sum => arr.min + arr.max
      }
}
