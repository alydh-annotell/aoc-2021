import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class DiveTest extends AnyFunSuite {

  val instructions = List(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  )

  test("Parse instructions with multiply") {
    assert(Dive.solve1(instructions) == 150)
  }

  test("task 1") {
    val bufferedSource = Source.fromFile("src/test/scala/input.txt")
    val fileInstructions = bufferedSource.getLines.toList
    bufferedSource.close
    println(Dive.solve1(fileInstructions))

  }

  test("Parse instructions with multiply 2") {
    assert(Dive.solve2(instructions) == 900)
  }

  test("task 2") {
    val bufferedSource = Source.fromFile("src/test/scala/input.txt")
    val fileInstructions = bufferedSource.getLines.toList
    bufferedSource.close
    println(Dive.solve2(fileInstructions))

  }
}
