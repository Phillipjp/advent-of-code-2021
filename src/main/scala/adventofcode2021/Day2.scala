package adventofcode2021

object Day2 extends App {

  def parseInstructions(instructions: List[String]): List[(String, Int)] = {
    instructions.map { i =>
      val split = i.split(" ")
      (split.head, split.last.toInt)
    }
  }

  def partOne(instructions: List[(String, Int)]): (Int, Int) = {
    instructions.foldLeft(0 -> 0) { (z, instruction) =>
      instruction match {
        case ("forward", x) => (z._1 + x) -> z._2
        case ("up", y) => z._1 -> (z._2 - y)
        case ("down", y) => z._1 -> (z._2 + y)
      }
    }
  }

  def partTwo(instructions: List[(String, Int)]): (Int, Int, Int) = {
    instructions.foldLeft((0,0,0)) { (z, instruction) =>
      instruction match {
        case ("forward", x) => (z._1 + x,  z._2 + z._3*x, z._3)
        case ("up", y) => (z._1, z._2, z._3 - y)
        case ("down", y) => (z._1, z._2, z._3 + y)
      }
    }
  }

  val instructions = parseInstructions(Utils.readFileAsListOfString("day-2-part-1.txt"))
  val partOneAnswer = partOne(instructions)
  println(s"Part One: $partOneAnswer => ${partOneAnswer._1 * partOneAnswer._2}")

  val partTwoAnswer = partTwo(instructions)
  println(s"Part Two: $partTwoAnswer => ${partTwoAnswer._1 * partTwoAnswer._2}")




}
