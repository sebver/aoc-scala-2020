object Day8 extends Day(8) {

  def run1() =
    run(parsed)

  def run2() = {
    val indexesWithJumps =
      parsed.zipWithIndex
        .filter { case ((ins, _), _) => ins == "jmp" }
        .map(_._2)

    val allCombinations = indexesWithJumps.map(index => parsed.updated(index, ("nop", 0)))

    LazyList
      .from(allCombinations)
      .map(run(_))
      .collectFirst {
        case (completed, global) if completed => global
      }
  }

  def parsed =
    readLines
      .map(_.split(" "))
      .map(arr => (arr(0), arr(1).toInt))
      .toArray

  def run(
      instructionSet: Array[(String, Int)],
      instructionIndex: Int = 0,
      executed: Set[Int] = Set.empty,
      global: Int = 0
  ): (Boolean, Int) =
    if (executed.contains(instructionIndex)) {
      (false, global)
    } else if (instructionIndex >= instructionSet.length) {
      (true, global)
    } else {
      val newExecuted = executed + instructionIndex
      instructionSet(instructionIndex) match {
        case ("nop", _) =>
          run(instructionSet, instructionIndex + 1, newExecuted, global)
        case ("acc", acc) =>
          run(instructionSet, instructionIndex + 1, newExecuted, global + acc)
        case ("jmp", jump) =>
          run(instructionSet, instructionIndex + jump, newExecuted, global)
      }
    }
}
