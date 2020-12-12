object Day6 extends Day(6) {

  def run1() =
    splitAtBlanks(readLines)
      .map(_.mkString.distinct.length.toLong)
      .sum

  def run2() =
    splitAtBlanks(readLines)
      .map(_.map(_.toCharArray.toSet))
      .map(sets => sets.foldLeft(sets.head)((set, curr) => set.intersect(curr)).size.toLong)
      .sum
}
