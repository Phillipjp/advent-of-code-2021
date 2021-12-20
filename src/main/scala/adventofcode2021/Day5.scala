package adventofcode2021

import scalaz.Scalaz._

import scala.annotation.tailrec


object Day5 extends App {

  def parseInput(input: List[String]): List[Line] = {
    input.map(makeLine)
  }

  def makeLine(i: String): Line = {
    val startEnd = i.split(" -> ")

    val start = startEnd.head.split(",")
    val startCoord = Coord(start.head.toInt, start.last.toInt)

    val end = startEnd.last.split(",")
    val endCoord = Coord(end.head.toInt, end.last.toInt)

    Line(startCoord, endCoord)
  }

  def mapVents(lines: List[Line]): Map[(Int, Int), Int] = {

    @tailrec
    def mapOut(lines: List[Line], vents: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
      lines match {
        case Nil => vents
        case line :: tail =>
//          val a = Seq(line.start.x, line.end.x).max - Seq(line.start.x, line.end.x).min
//          val b = Seq(line.start.y, line.end.y).max - Seq(line.start.y, line.end.y).min
//          val length = Math.sqrt((a * a) + (b * b)) + 1

          val length = Seq(
            Seq(line.start.x, line.end.x).max - Seq(line.start.x, line.end.x).min,
            Seq(line.start.y, line.end.y).max - Seq(line.start.y, line.end.y).min
          ).max + 1

          println(line)
          println(s"length: $length")

          if (line.start.x == line.end.x && line.start.y < line.end.y) {
            val lineMap = makeLineMap(line.start, 0, 1, length.toInt)
            mapOut(tail, vents |+| lineMap)
          }
          else if (line.start.x == line.end.x && line.start.y > line.end.y) {
            val lineMap = makeLineMap(line.start, 0, -1, length.toInt)
            mapOut(tail, vents |+| lineMap)
          }
          else if (line.start.y == line.end.y && line.start.x < line.end.x) {
            val lineMap = makeLineMap(line.start, 1, 0, length.toInt)
            mapOut(tail, vents |+| lineMap)
          }
          else if (line.start.y == line.end.y && line.start.x > line.end.x) {
            val lineMap = makeLineMap(line.start, -1, 0, length.toInt)
            mapOut(tail, vents |+| lineMap)
          }
          else if (line.start.x > line.end.x && line.start.y > line.end.y){
            val lineMap = makeLineMap(line.start, -1, -1, length.toInt)
            mapOut(tail, vents |+| lineMap)
          }
          else if (line.start.x > line.end.x && line.start.y < line.end.y){
            val lineMap = makeLineMap(line.start, -1, 1, length.toInt)
            mapOut(tail, vents |+| lineMap)
          }
          else if (line.start.x < line.end.x && line.start.y > line.end.y){
            val lineMap = makeLineMap(line.start, 1, -1, length.toInt)
            mapOut(tail, vents |+| lineMap)
          }
          else {
            val lineMap = makeLineMap(line.start, 1, 1, length.toInt)
            mapOut(tail, vents |+| lineMap)
          }
//        else{
//            mapOut(tail, vents)
//          }


      }
    }

    mapOut(lines, Map[(Int, Int), Int]())
  }


  def makeLineMap(start: Coord, xIncrement: Int, yIncrement: Int, length: Int): Map[(Int, Int), Int] = {
    Stream.iterate(((start.x, start.y), 1))(vent => nextVent(vent, xIncrement, yIncrement))
      .take(length)
//            .takeWhile(vent => vent._1._1 <= end.x && vent._1._2 <= end.y)
      .toMap
  }

  def nextVent(vent: ((Int, Int), Int), xIncrement: Int, yIncrement: Int): ((Int, Int), Int) = {
    ((vent._1._1 + xIncrement, vent._1._2 + yIncrement), 1)
  }

  val input = Utils.readFileAsListOfString("day-5-part-1.txt")
  val lines = parseInput(input)
  val ventMap = mapVents(lines)
  val partOneAnswer = ventMap.values.count(_ > 1)
  println(partOneAnswer)

}

case class Coord(x: Int, y: Int)

case class Line(start: Coord, end: Coord)
