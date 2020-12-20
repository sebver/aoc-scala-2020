import scala.annotation.tailrec

object Day12 extends Day(12) {

  def run1() = {
    move(Position(0, 0), East, parsed).manhattanFromOrigin
  }

  def run2() = {
    moveWaypoint(Position(0, 0), Position(10, 1), parsed).manhattanFromOrigin
  }

  def parsed: List[Action] = readLines
    .map {
      e =>
        val splitted = e.splitAt(1)
        (splitted._1.head, splitted._2.toInt)
    }
    .map(toAction)

  def toAction(input: (Char, Int)): Action = input match {
    case ('N', n) => Move(North, n)
    case ('E', n) => Move(East, n)
    case ('S', n) => Move(South, n)
    case ('W', n) => Move(West, n)
    case ('L', d) => Turn(Left, d)
    case ('R', d) => Turn(Right, d)
    case ('F', n) => Forward(n)
    case _        => throw new IllegalArgumentException
  }

  case class Position(x: Int, y: Int) {
    def addX(x2: Int): Position = copy(x = x + x2)

    def addY(y2: Int): Position = copy(y = y + y2)

    def go(n: Int, direction: Direction): Position = direction match {
      case North => addY(n)
      case East  => addX(n)
      case South => addY(-n)
      case West  => addX(-n)
    }

    def rotate(rotation: Rotation, n: Int): Position = {
      val flip = if (rotation == Right) 1 else -1
      n match {
        case 90  => copy(x = flip * y, y = (-flip) * x)
        case 180 => copy(x = -x, y = -y)
        case 270 => copy(x = (-flip) * y, y = flip * x)
      }
    }

    def manhattanFromOrigin: Int = Math.abs(x) + Math.abs(y)
  }

  sealed trait Action
  case class Move(direction: Direction, n: Int)     extends Action
  case class Turn(rotation: Rotation, degrees: Int) extends Action
  case class Forward(n: Int)                        extends Action

  sealed trait Direction
  case object North extends Direction
  case object East  extends Direction
  case object South extends Direction
  case object West  extends Direction

  sealed trait Rotation
  case object Left  extends Rotation
  case object Right extends Rotation

  @tailrec
  def move(current: Position, direction: Direction, remaining: List[Action]): Position =
    remaining match {
      case Move(direction, n) :: rem =>
        move(current.go(n, direction), direction, rem)
      case Turn(rotation, degrees) :: rem =>
        move(current, turn(rotation, degrees, direction), rem)
      case Forward(n) :: rem =>
        move(current.go(n, direction), direction, rem)
      case Nil =>
        current
    }

  @tailrec
  def moveWaypoint(current: Position, waypoint: Position, remaining: List[Action]): Position =
    remaining match {
      case Move(direction, n) :: rem =>
        moveWaypoint(current, waypoint.go(n, direction), rem)
      case Turn(rotation, n) :: rem =>
        moveWaypoint(current, waypoint.rotate(rotation, n), rem)
      case Forward(n) :: rem =>
        moveWaypoint(current.addX(waypoint.x * n).addY(waypoint.y * n), waypoint, rem)
      case Nil =>
        current
    }

  val directions: Array[Direction] = Array(North, East, South, West)

  def turn(rotation: Rotation, degrees: Int, currentDirection: Direction): Direction = {
    val flip = if (rotation == Right) 1 else -1
    val idx  = (directions.indexOf(currentDirection) + flip * (degrees / 90)) % 4
    directions(if (idx < 0) 4 + idx else idx)
  }
}
