object Day3 extends Day(3) {

  def run1() =
    countTrees(readChars, 3, 1)

  def run2() = {
    val chars = readChars

    countTrees(chars, 1, 1) *
      countTrees(chars, 3, 1) *
      countTrees(chars, 5, 1) *
      countTrees(chars, 7, 1) *
      countTrees(chars, 1, 2)
  }

  def countTrees(chars: Array[Array[Char]], right: Int, down: Int) = {
    var total = 0L
    var index = 0

    for (row <- chars.indices by down) {
      if (chars(row)(index) == '#') {
        total = total + 1
      }
      index = (index + right) % chars(0).length
    }

    total
  }

  def readChars = readLines.map(_.toCharArray).toArray
}
