object Dive {
  def solve1(instructions: List[String]): Int =
    instructions
      .map(parseInstruction1)
      .reduce(accPosition1)
      .multiply

  def accPosition1(acc: position, elem: position): position =
    position(acc.hor + elem.hor, acc.depth + elem.depth)

  def parseInstruction1(instruction: String): position =
    instruction.split(" ") match {
      case Array("forward", x) => position(hor = x.toInt)
      case Array("up", x)      => position(depth = -x.toInt)
      case Array("down", x)    => position(depth = x.toInt)
    }

  def solve2(instructions: List[String]): Int =
    instructions
      .map(parseInstruction2)
      .reduce(accPosition2)
      .multiply

  def accPosition2(acc: position, elem: position): position =
    position(
      acc.hor + elem.hor,
      acc.depth + (acc.aim * elem.hor),
      acc.aim + elem.aim
    )

  def parseInstruction2(instruction: String): position =
    instruction.split(" ") match {
      case Array("forward", x) => position(hor = x.toInt)
      case Array("up", x)      => position(aim = -x.toInt)
      case Array("down", x)    => position(aim = x.toInt)
    }

  case class position(hor: Int = 0, depth: Int = 0, aim: Int = 0) {
    def multiply: Int = this.hor * this.depth
  }

}
