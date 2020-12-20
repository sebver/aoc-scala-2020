import scala.annotation.tailrec
import scala.collection.mutable

object Day18 extends Day(18) {

  def run1() = readLines.map(result1).sum

  def run2() = readLines.map(result2).sum

  def result1(expression: String) = calc(parse(expression))

  def result2(expression: String) = calc(insertBracketsAroundAddition(parse(expression)))

  def calc(expressions: List[Expr], stack: mutable.Stack[Expr] = mutable.Stack.empty): Long = {
    if (expressions.isEmpty) {
      stack.popAll match {
        case Value(value) :: Nil =>
          value
        case _ =>
          throw new Exception("Expected single value")
      }
    } else {
      expressions.head match {
        case Addition | Multiplication | OpenBracket =>
          stack.push(expressions.head)
        case Value(value) =>
          stack.push(Value(value))
          solveStack(stack)
        case CloseBracket =>
          (stack.pop, stack.pop) match {
            case (Value(value), OpenBracket) =>
              stack.push(Value(value))
              solveStack(stack)
            case (first, second) =>
              throw new Exception(s"Bad bracket close: $first $second")
          }
      }

      calc(expressions.tail, stack)
    }
  }

  // Will only solve value/op/value , eg 2 + 3
  def solveStack(stack: mutable.Stack[Expr]): mutable.Stack[Expr] =
    if (stack.isEmpty) stack
    else {
      stack.pop match {
        case Value(value) if stack.nonEmpty =>
          stack.top match {
            case Addition | Multiplication =>
              stack.push((stack.pop, stack.pop) match {
                case (Addition, Value(firstValue)) =>
                  Value(value + firstValue)
                case (Multiplication, Value(firstValue)) =>
                  Value(value * firstValue)
                case _ =>
                  throw new Exception("Expected value to precede operator")
              })
              solveStack(stack)
            case _ =>
              stack.push(Value(value))
          }
        case other =>
          stack.push(other)
      }
    }

  @tailrec
  def insertBracketsAroundAddition(expression: List[Expr], startIndex: Int = 0): List[Expr] = {
    val firstAddition = expression.indexOf(Addition, startIndex)
    if (firstAddition == -1)
      expression
    else {
      val left    = expression.slice(0, firstAddition)
      val leftOp  = getLeftExpression(left)
      val right   = expression.slice(firstAddition + 1, expression.length)
      val rightOp = getRightExpression(right)

      insertBracketsAroundAddition(
        left.dropRight(leftOp.size) :::
          List(OpenBracket) :::
          leftOp :::
          List(Addition) :::
          rightOp :::
          List(CloseBracket) :::
          right.drop(rightOp.size),
        firstAddition + 1 + 1 // +1 for added bracket, +1 because we need to start searching after this index
      )
    }
  }

  @tailrec
  def getLeftExpression(
      remaining: List[Expr],
      current: List[Expr] = List.empty,
      bracketsLeft: Int = 0
  ): List[Expr] =
    remaining.last match {
      case Value(value) if bracketsLeft == 0 =>
        List(Value(value))
      case CloseBracket =>
        getLeftExpression(remaining.dropRight(1), CloseBracket :: current, bracketsLeft + 1)
      case OpenBracket if bracketsLeft == 1 =>
        OpenBracket :: current
      case OpenBracket =>
        getLeftExpression(remaining.dropRight(1), OpenBracket :: current, bracketsLeft - 1)
      case other =>
        getLeftExpression(remaining.dropRight(1), other :: current, bracketsLeft)
    }

  @tailrec
  def getRightExpression(
      remaining: List[Expr],
      current: List[Expr] = List.empty,
      bracketsLeft: Int = 0
  ): List[Expr] =
    remaining.head match {
      case Value(value) if bracketsLeft == 0 =>
        List(Value(value))
      case OpenBracket =>
        getRightExpression(remaining.drop(1), current :+ OpenBracket, bracketsLeft + 1)
      case CloseBracket if bracketsLeft == 1 =>
        current :+ CloseBracket
      case CloseBracket =>
        getRightExpression(remaining.drop(1), current :+ CloseBracket, bracketsLeft - 1)
      case other =>
        getRightExpression(remaining.drop(1), current :+ other, bracketsLeft)
    }

  def parse(expression: String) = parseWithoutWhitespace(expression.replace(" ", ""))

  def parseWithoutWhitespace(expression: String): List[Expr] = {
    val (expr, remainder) = parseFirst(expression)
    if (remainder.nonEmpty)
      expr :: parse(remainder)
    else
      List(expr)
  }

  def parseFirst(expression: String): (Expr, String) = expression.head match {
    case '(' => (OpenBracket, expression.drop(1))
    case ')' => (CloseBracket, expression.drop(1))
    case '+' => (Addition, expression.drop(1))
    case '*' => (Multiplication, expression.drop(1))
    case _ =>
      val number = expression.takeWhile(_.isDigit)
      (Value(number.toLong), expression.stripPrefix(number))
  }

  sealed trait Expr
  case object OpenBracket       extends Expr
  case object CloseBracket      extends Expr
  case object Addition          extends Expr
  case object Multiplication    extends Expr
  case class Value(value: Long) extends Expr
}
