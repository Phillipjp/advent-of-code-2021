package adventofcode2021

object Day1 extends App{

  def partOne(depths: List[Int]): Int = {
    depths.sliding(2, 1).count(l => l.last > l.head)
  }

  def partTwo(depths: List[Int]): Int = {
    val summedWindowedDepths = depths.sliding(3).map(_.sum).toList
    partOne(summedWindowedDepths)
  }


  val depths = Utils.readFileAsListOfInt("day-1-part-1.txt")
  val partOneIncreases = partOne(depths)
  println(s"Part One Increases: $partOneIncreases")

  val partTwoIncreases = partTwo(depths)
  println(s"Part Two Increases: $partTwoIncreases")


}
