package adventofcode2021

object Day13 extends App {

  val points = Utils.readFileAsListOfString("day-13-points.txt")
    .map{line =>
      val split = line.split(",")
      (split.head.toInt, split.last.toInt)
    }

  val instructions = Utils.readFileAsListOfString("day-13-instructions.txt").map{line =>
    val i = line.split(" ").last.split("=")
    (i.head, i.last.toInt)
  }

  val answer1 = Seq(instructions.head).foldLeft(points){case(points, (axis, number)) =>
    axis match {
      case "x" => foldXAxis(points, number)
      case "y" =>foldYAxis(points, number)
    }
  }.size

  println(answer1)

  val answer2 = instructions.foldLeft(points){case(points, (axis, number)) =>
    axis match {
      case "x" => foldXAxis(points, number)
      case "y" =>foldYAxis(points, number)
    }
  }

  val maxX = answer2.map(_._1).max
  val maxY = answer2.map(_._2).max


  for(y <- 0 to maxY){
    for(x <- 0 to maxX){
      if(answer2.contains((x,y))) print(" # ") else print(" . ")
    }
    println()
  }

  def foldXAxis(points: List[(Int, Int)], number: Int):  List[(Int, Int)] = {
    val samePoints = points.filter(_._1 < number)
    val newPoints = points.filter(_._1 > number).map{case(x,y) => (number - (x-number), y)}

    (samePoints ++ newPoints).distinct
  }

  def foldYAxis(points: List[(Int, Int)], number: Int):  List[(Int, Int)] = {
    val samePoints = points.filter(_._2 < number)
    val newPoints = points.filter(_._2 > number).map{case(x,y) => (x, number - (y-number))}

    (samePoints ++ newPoints).distinct
  }
}
