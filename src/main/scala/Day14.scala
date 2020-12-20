import java.lang.Long.parseLong

object Day14 extends Day(14) {

  val example = """mask = 000000000000000000000000000000X1001X
                  |mem[42] = 100
                  |mask = 00000000000000000000000000000000X0XX
                  |mem[26] = 1""".stripMargin.split("\n").toList

  def run1() = {
    readBlocks(readLines)
      .foldLeft(Map.empty[Long, Long]) {
        (memory, block) =>
          executeBlock(memory, block)
      }
      .values
      .sum
  }

  def executeBlock(memory: Map[Long, Long], block: Block) = {
    block.instructions.foldLeft(memory) {
      (memory, instruction) =>
        {
          val newValue = (instruction.value | block.mask.maskOr) & block.mask.maskAnd
          memory.updated(instruction.address, newValue)
        }
    }
  }

  def run2() = {
    readBlocks(readLines)
      .foldLeft(Map.empty[Long, Long]) {
        (memory, block) =>
          executeBlock2(memory, block)
      }
      .values
      .sum
  }

  def executeBlock2(memory: Map[Long, Long], block: Block) = {
    block.instructions.foldLeft(memory) {
      (memory, instruction) =>
        val addressOr = instruction.address | block.mask.maskOr
        val addressFloating =
          longToBitArrayString(addressOr).zipWithIndex.map {
            case (_, i) if block.mask.mask.charAt(i) == 'X' =>
              'X'
            case (c, _) =>
              c
          }.mkString

        replaceFloating(addressFloating).map(parseLong(_, 2)).foldLeft(memory) {
          (m, address) =>
            m.updated(address, instruction.value)
        }
    }
  }

  def longToBitArrayString(input: Long): String =
    List.fill(36 - input.toBinaryString.length)('0').mkString + input.toBinaryString

  def replaceFloating(mask: String): Set[String] = mask.indexOf('X') match {
    case -1 => Set(mask)
    case i  => replaceFloating(mask.updated(i, '0')) union replaceFloating(mask.updated(i, '1'))
  }

  def readBlocks(lines: List[String]): Array[Block] = {
    val regex = "mem\\[(\\d+)\\] = (\\d+)".r

    lines.mkString("\n").split("mask = ").filterNot(_.isEmpty).map {
      block =>
        val blockLines = block.split("\n")
        val instructions = blockLines
          .drop(1)
          .collect {
            case regex(address, value) =>
              Instruction(address.toLong, value.toLong)
          }
          .toList
        Block(Mask(blockLines(0)), instructions)
    }
  }

  case class Block(mask: Mask, instructions: List[Instruction])

  case class Instruction(address: Long, value: Long)

  case class Mask(mask: String) {
    val maskOr  = parseLong(mask.replace('X', '0'), 2)
    val maskAnd = parseLong(mask.replace('X', '1'), 2)
  }
}
