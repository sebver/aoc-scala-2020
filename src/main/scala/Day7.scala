import cats.implicits._

object Day7 extends Day(7) {

  def run1() = {
    val parentsPerBagType: Map[String, Set[String]] =
      readLines
        .map(parseLine)
        .map {
          case (parent, children) => children.map { case (_, child) => (child, Set(parent)) }.toMap
        }
        .combineAll

    def parentSets(bagType: String): List[Set[String]] =
      parentsPerBagType(bagType).toList
        .flatMap {
          parentBag =>
            if (parentsPerBagType.contains(parentBag))
              parentSets(parentBag).map(_ union Set(parentBag))
            else
              List(Set(parentBag))
        }

    parentSets("shiny gold").combineAll.size
  }

  def run2() = {
    val bagChildrenMap: Map[String, List[(Long, String)]] =
      readLines.map(parseLine).filter(_._2.nonEmpty).toMap

    def totalCount(bagType: String): Long =
      if (!bagChildrenMap.contains(bagType))
        1L
      else
        1L + bagChildrenMap(bagType).map {
          case (count, childBag) =>
            count * totalCount(childBag)
        }.sum

    totalCount("shiny gold") - 1
  }

  def parseLine(input: String) = {
    val s1 = input.stripSuffix(".").split(" bags contain ")
    val s2 = s1(1)
      .split(",")
      .map(_.trim)
      .map(str => str.splitAt(str.indexOf(' ')))
      .collect {
        case (l, r) if l != "no" => (l.toLong, r.trim.replace(" bags", "").replace(" bag", ""))
      }
      .toList

    (s1(0), s2)
  }
}
