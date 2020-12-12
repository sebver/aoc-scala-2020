import atto._
import Atto._

object Day2 extends Day(2) {

  case class PasswordData(min: Int, max: Int, letter: Char, password: String)

  def run2(): Int =
    parsed.count {
      p =>
        val first  = p.password.charAt(p.min - 1)
        val second = p.password.charAt(p.max - 1)
        first == p.letter ^ second == p.letter
    }

  def run1(): Int =
    parsed.count {
      p =>
        val totalCount = p.password.count(_ == p.letter)
        totalCount >= p.min && totalCount <= p.max
    }

  val parser = for {
    min      <- int
    _        <- char('-')
    max      <- int
    _        <- whitespace
    c        <- letter
    _        <- string(": ")
    password <- many(letter)
  } yield PasswordData(min, max, c, password.mkString)

  def parsed = readLines.map(parseInput)

  def parseInput(input: String) = parser.parseOnly(input).option.get

}
