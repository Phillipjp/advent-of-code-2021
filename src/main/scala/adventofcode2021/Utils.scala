package adventofcode2021

import scala.io.Source


object Utils {

  def readFileAsListOfString(fileName: String): List[String] = {
    Source.fromResource(fileName).getLines().toList
  }

  def readFileAsListOfInt(fileName: String): List[Int] = {
    readFileAsListOfString(fileName).map(_.toInt)
  }

}
