object Day19 extends Day(19) with MainPart2 {

  def run1() = {
    val lines   = splitAtBlanks(readLines)
    val ruleMap = parse(lines.head.toList)
    lines(1).count(validate(_, ruleMap))
  }

  def run2() = runPart2(readLines)

  def runPart2(all: List[String]) = {
    val lines   = splitAtBlanks(all)
    val ruleMap = updateRulesWithLoops(parse(lines.head.toList))
    lines(1).count(validate(_, ruleMap))
  }

  def validate(input: String, rules: Map[Int, Rule]): Boolean = {
    val result = rules(0).validate(input, rules)
    result.exists(_.contains(""))
  }

  def updateRulesWithLoops(ruleMap: Map[Int, Rule]) = ruleMap
    .updated(8, Rule.Or(8, Rule.Sequence(-1, List(42)), Rule.Sequence(-1, List(42, 8))))
    .updated(
      11,
      Rule.Or(11, Rule.Sequence(-1, List(42, 31)), Rule.Sequence(-1, List(42, 11, 31)))
    )

  def parse(rules: List[String]) = rules.map(parseRule).map(r => (r.id, r)).toMap

  val outer = "(\\d+): (.+)".r
  def parseRule(str: String) = str match {
    case outer(idStr, content) =>
      val id = idStr.toInt
      content match {
        case "\"a\"" => Rule.Text(id, "a")
        case "\"b\"" => Rule.Text(id, "b")
        case other =>
          val sequences: Array[List[Int]] =
            other.split(" \\| ").map(_.split(" ").map(_.toInt).toList)
          if (sequences.length == 1)
            Rule.Sequence(id, sequences.head)
          else
            Rule.Or(
              id,
              Rule.Sequence(-1, sequences.head),
              Rule.Sequence(-1, sequences.drop(1).head)
            )
      }
  }

  sealed trait Rule {
    val id: Int

    // If it was validated correctly, returns a list of remainders
    def validate(input: String, ruleMap: Map[Int, Rule]): Option[List[String]]
  }

  object Rule {
    case class Text(id: Int, text: String) extends Rule {
      override def validate(input: String, ruleMap: Map[Int, Rule]): Option[List[String]] = {
        if (input.startsWith(text))
          Some(List(input.stripPrefix(text)))
        else
          None
      }
    }

    case class Sequence(id: Int, rules: List[Int]) extends Rule {
      override def validate(input: String, ruleMap: Map[Int, Rule]): Option[List[String]] =
        rules.foldLeft(Option(List(input))) {
          (remainders, ruleId) =>
            remainders match {
              case Some(rem) =>
                val results = rem.flatMap(r => ruleMap(ruleId).validate(r, ruleMap)).flatten
                if (results.nonEmpty)
                  Some(results)
                else
                  None
              case None =>
                None
            }
        }
    }

    case class Or(id: Int, left: Sequence, right: Sequence) extends Rule {
      override def validate(input: String, ruleMap: Map[Int, Rule]): Option[List[String]] = {
        val leftRemainders  = left.validate(input, ruleMap)
        val rightRemainders = right.validate(input, ruleMap)

        (leftRemainders, rightRemainders) match {
          case (Some(l), Some(r)) =>
            Some(l ::: r)
          case (Some(l), _) =>
            Some(l)
          case (_, Some(r)) =>
            Some(r)
          case _ => None
        }
      }
    }
  }
}
