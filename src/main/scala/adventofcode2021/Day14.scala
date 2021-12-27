package adventofcode2021

import scalaz.Scalaz._

import scala.annotation.tailrec

object Day14 extends App {

  val input = Utils.readFileAsListOfString("day-14.txt")
  val template = input.head
  val rules = input.drop(2).map { line =>
    val split = line.split(" -> ")
    split.head -> split.last
  }
    .toMap

  val finalPolymer = growPolymer(template, rules, 10)

  val leastCommonElement = leastCommonLetter(finalPolymer)
  val mostCommonElement = mostCommonLetter(finalPolymer)

  val leastCommonElementCount = finalPolymer.count(_ == leastCommonElement)
  val mostCommonElementCount = finalPolymer.count(_ == mostCommonElement)
  println(mostCommonElementCount - leastCommonElementCount)

  def growPolymer(template: String, rules: Map[String, String], turns: Int): String = {
    @tailrec
    def grow(template: String, turns: Int, i: Int): String = {
      if(i == turns)
        template
      else{
        val windowed = template.sliding(2, 1)
        grow(windowed.map(pair => pair.head + rules(pair)).mkString + template.last, turns, i+1)
      }
    }

    grow(template, turns, 0)
  }


  def mostCommonLetter(string:String): Char = {
    string.groupBy(c => c).mapValues(_.length).maxBy(_._2)._1
  }
  def leastCommonLetter(string:String): Char = {
    string.groupBy(c => c).mapValues(_.length).minBy(_._2)._1
  }
}
