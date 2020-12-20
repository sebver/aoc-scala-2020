import scala.annotation.tailrec

object Day16 extends Day(16) {

  val example = """class: 0-1 or 4-19
                  |row: 0-5 or 8-19
                  |seat: 0-13 or 16-19
                  |
                  |your ticket:
                  |11,12,13
                  |
                  |nearby tickets:
                  |3,9,18
                  |15,1,5
                  |5,14,9""".stripMargin.split("\n").toList

  case class Field(name: String, ranges: Array[(Int, Int)]) {
    def isValid(value: Int): Boolean = ranges.exists(r => r._1 <= value && value <= r._2)
  }

  case class Ticket(numbers: Array[Int])

  case class Input(fields: Array[Field], ticket: Ticket, nearby: Array[Ticket])

  def run1() = {
    val input = parse(readLines)
    input.nearby
      .map(invalidNumbers(_, input).sum)
      .sum
  }

  def invalidNumbers(ticket: Ticket, input: Input): Array[Int] =
    ticket.numbers.filter(number => !input.fields.exists(_.isValid(number)))

  def run2() = {
    val input      = parse(readLines)
    val validInput = input.copy(nearby = input.nearby.filter(isValidTicket(_, input)))
    val results    = getFieldsWithOrder(validInput)
    assert(results.values.toSet.size == input.ticket.numbers.length)
    val departureIndexes = results.filter(_._1.name.startsWith("departure")).values
    departureIndexes
      .map(input.ticket.numbers(_).toLong)
      .product
  }

  def isValidTicket(ticket: Ticket, input: Input): Boolean =
    ticket.numbers.forall(number => input.fields.exists(_.isValid(number)))

  @tailrec
  def getFieldsWithOrder(input: Input, found: Map[Field, Int] = Map.empty): Map[Field, Int] = {
    input.fields.filterNot(found.contains).toList match {
      case Nil =>
        found
      case remainingFields =>
        val indexesToCheck =
          input.ticket.numbers.indices.filterNot(found.values.toSet.contains).toList

        val iterationResults = remainingFields
          .map(f => (f, indexesToCheck.filter(isViableAtIndex(input.nearby, f, _))))
          .collect {
            case (f, single :: Nil) =>
              (f, single)
          }

        getFieldsWithOrder(input, (found.toSeq ++ iterationResults).toMap)
    }
  }

  def isViableAtIndex(tickets: Array[Ticket], field: Field, index: Int): Boolean =
    tickets.map(_.numbers(index)).forall(field.isValid)

  val fieldRegex = "([a-z ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r
  def parse(lines: List[String]): Input = {
    val splitted = splitAtBlanks(lines)
    val fields = splitted(0) collect {
      case fieldRegex(name, a, b, c, d) =>
        Field(name, Array((a.toInt, b.toInt), (c.toInt, d.toInt)))
    }
    val ticket = splitted(1)(1).split(",").map(_.toInt)
    val nearby = splitted(2).drop(1).map(_.split(",").map(_.toInt))
    Input(fields, Ticket(ticket), nearby.map(Ticket))
  }
}
