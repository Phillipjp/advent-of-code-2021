package adventofcode2021

object Day2 extends App {

  def parseInstructions(instructions: List[String]): List[(String, Int)] = {
    instructions.map { i =>
      val split = i.split(" ")
      (split.head, split.last.toInt)
    }
  }

  def followInstructions[T](instructions: List[(String, Int)], initialValue: T, completeInstruction: (T, (String, Int)) => T): T = {
    instructions.foldLeft(initialValue) { (z, instruction) => completeInstruction(z, instruction)}
  }

  lazy val completePartOneInstruction: ((Int, Int), (String, Int)) => (Int, Int) = (z, instruction) =>
    instruction match {
      case ("forward", x) => (z._1 + x) -> z._2
      case ("up", y) => z._1 -> (z._2 - y)
      case ("down", y) => z._1 -> (z._2 + y)
    }

  lazy val completePartTwoInstruction: ((Int, Int, Int), (String, Int)) => (Int, Int, Int) = (z, instruction) =>
    instruction match {
      case ("forward", x) => (z._1 + x,  z._2 + z._3*x, z._3)
      case ("up", y) => (z._1, z._2, z._3 - y)
      case ("down", y) => (z._1, z._2, z._3 + y)
    }

  val instructions = parseInstructions(Utils.readFileAsListOfString("day-2.txt"))
  val partOneAnswer = followInstructions(instructions, (0,0), completePartOneInstruction)
  println(s"Part One: $partOneAnswer => ${partOneAnswer._1 * partOneAnswer._2}")

  val partTwoAnswer = followInstructions(instructions, (0,0,0), completePartTwoInstruction)
  println(s"Part Two: $partTwoAnswer => ${partTwoAnswer._1 * partTwoAnswer._2}")




}
