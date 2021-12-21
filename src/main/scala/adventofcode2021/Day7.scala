package adventofcode2021

object Day7 extends App{

  val input = Utils.readFileAsListOfString("day-7.txt").head.split(",").toList.map(_.toInt)


  val result = (0 to input.max).foldLeft((-1, Int.MaxValue)){case ((i, min), horizontalPosition) =>
   val diffsSum = input.map{crab =>
     Seq(horizontalPosition, crab).max - Seq(horizontalPosition, crab).min
   }.sum
    if(diffsSum < min)
      (horizontalPosition, diffsSum)
    else
      (i, min)
  }

  println(result)

  val result2 = (0 to input.max).foldLeft((-1, Int.MaxValue)){case ((i, min), horizontalPosition) =>
    val diffsSum = input.map{crab =>
      (1 to Seq(horizontalPosition, crab).max - Seq(horizontalPosition, crab).min).sum
    }.sum
    if(diffsSum < min)
      (horizontalPosition, diffsSum)
    else
      (i, min)
  }

  println(result2)

}
