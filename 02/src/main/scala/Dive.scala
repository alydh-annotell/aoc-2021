object Dive {
  def solve1(instructions: List[String]): Int =
    instructions
      .map(parseInstruction1)
      .reduce(accPosition1)
      .multiply

  def accPosition1(acc: position1, elem: position1): position1 =
    position1(acc.hor + elem.hor, acc.depth + elem.depth)

  def parseInstruction1(instruction: String): position1 =
    instruction.split(" ") match {
      case Array("forward", x) => position1(x.toInt, 0)
      case Array("up", x)      => position1(0, -x.toInt)
      case Array("down", x)    => position1(0, x.toInt)
    }

  def solve2(instructions: List[String]): Int =
    instructions
      .map(parseInstruction2)
      .reduce(accPosition2)
      .multiply

  def accPosition2(acc: position2, elem: position2): position2 =
    position2(
      acc.hor + elem.hor,
      acc.depth + (acc.aim * elem.hor),
      acc.aim + elem.aim
    )

  def parseInstruction2(instruction: String): position2 =
    instruction.split(" ") match {
      case Array("forward", x) => position2(x.toInt, 0, 0)
      case Array("up", x)      => position2(0, 0, -x.toInt)
      case Array("down", x)    => position2(0, 0, x.toInt)
    }

  case class position1(hor: Int, depth: Int) {
    def multiply: Int = this.hor * this.depth
  }

  case class position2(hor: Int, depth: Int, aim: Int) {
    def multiply: Int = this.hor * this.depth
  }

}
