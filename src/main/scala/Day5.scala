object Day5 extends Day(5) {

  def run1() = readLines.map(toSeat).map(seatId).max

  def run2() =
    readLines
      .map(toSeat)
      .map(seatId)
      .sorted
      .sliding(2)
      .collectFirst {
        case first :: second :: Nil if second - first == 2 => first + 1
      }

  def seatId(input: (Int, Int)) = input._1 * 8L + input._2

  def toSeat(input: String) = (toRow(input.take(7)), toCol(input.drop(7)))

  def toRow(input: String) =
    parse(
      input,
      {
        case 'F' => '0'
        case 'B' => '1'
      }
    )

  def toCol(input: String) =
    parse(
      input,
      {
        case 'L' => '0'
        case 'R' => '1'
      }
    )

  def parse(input: String, convert: Char => Char) =
    Integer.parseInt(input.map(convert), 2)

}
