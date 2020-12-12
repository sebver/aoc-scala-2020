trait MainPart1 {

  def run1(): Any

  def main(args: Array[String]): Unit = pprint.pprintln(run1())
}

trait MainPart2 {

  def run2(): Any

  def main(args: Array[String]): Unit = pprint.pprintln(run2())
}
