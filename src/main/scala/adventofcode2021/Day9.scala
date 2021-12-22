package adventofcode2021

import scala.annotation.tailrec

object Day9 extends App{

  val input = Utils.readFileAsListOfString("day-9.txt")
  val heightMap = makeHeightMap(input)
  val lowPoints = findLowPoints(heightMap)
  val riskLevel = lowPoints.size + lowPoints.sum
  println(riskLevel)
  val basins = findBasins(heightMap)
  val basinRiskLevel = basins.sorted.takeRight(3).product
  println(basinRiskLevel)

  def makeHeightMap(input: List[String]): Map[(Int, Int), Int] =
    input.zipWithIndex
      .flatMap { case (line, y) =>
        line.zipWithIndex.map { case (height, x) => (x, y) -> height.toString.toInt }
      }
      .toMap

  def findLowPoints(heightMap: Map[(Int, Int), Int]): List[Int] = {

    heightMap.foldLeft(List[Int]()){case (lowPoints, ((x,y), height)) =>
      val (above, below, left, right) = surroundingHeights(heightMap, x, y)

      if(Seq(above, below, left, right).flatten.forall(_ > height))
        lowPoints :+ height
      else
        lowPoints

    }
  }

  def findBasins(heightMap: Map[(Int, Int), Int]): List[Int] = {

    heightMap.foldLeft(List[Int]()){case (basins, ((x,y), height)) =>
      val (above, below, left, right) = surroundingHeights(heightMap, x, y)

      if(Seq(above, below, left, right).flatten.forall(_ > height))
        basins :+ getBasin(heightMap, x, y)
      else
        basins

    }
  }

  def surroundingHeights(heightMap: Map[(Int, Int), Int], x:Int, y: Int): (Option[Int], Option[Int], Option[Int], Option[Int]) = {
    val above = heightMap.get((x, y+1))
    val below = heightMap.get((x, y-1))
    val left = heightMap.get((x-1, y))
    val right = heightMap.get((x+1, y))

    (above, below, left, right)
  }

  def getBasin(heightMap: Map[(Int, Int), Int], x:Int, y: Int): Int = {
    @tailrec
    def basinSize(points: List[(Int, Int)], visitedPoints: List[(Int, Int)], size: Int): Int = {
      points match {
        case Nil => size
        case _ =>
          val (x,y) = points.head
          val (above, below, left, right) = surroundingHeights(heightMap, x, y)

          val basinPoints = Seq((above, (x, y+1)), (below, (x, y-1)), (left, (x-1, y)), (right, (x+1, y)))
            .filter(_._1.isDefined)
            .map {case (height, point) => height.get -> point}
            .filter(_._1 < 9)
            .map(_._2)

          val updatedPoints = (points.tail ++ basinPoints).distinct.filter(point => !visitedPoints.contains(point))
          val updatedVisitedPoints = visitedPoints :+ (x,y)
          basinSize(updatedPoints, updatedVisitedPoints, size + 1)

      }

    }
    basinSize(List((x, y)), List(), 0)
  }
}
