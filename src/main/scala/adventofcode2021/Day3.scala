package adventofcode2021

import scala.annotation.tailrec

object Day3 extends App{

  def calculateGammaRate(binary: List[String]): String = {
    binary.foldLeft(Stream.continually(0).take(binary.head.length)){ (z, bits) =>
      z.zip(bits).map{case (t, bit) => t + bit.toString.toInt}
    }
      .map(i =>  if(i >= binary.length.toDouble/2) '1' else '0')
      .mkString
  }

  def mostPopularBitAtIndex(binary:List[String], index: Int): Char = {
    val bits = binary.map(_.charAt(index).toString.toInt)
    if(bits.sum >= binary.length.toDouble/2) '1' else '0'
  }

  def leastPopularBitAtIndex(binary:List[String], index: Int): Char = {
    val bits = binary.map(_.charAt(index).toString.toInt)
    if(bits.sum >= binary.length.toDouble/2) '0' else '1'
  }

  def invertBinary(binary: String): String = {
    binary.map{bit =>if(bit == '0') '1' else '0'}
  }

  def calculatePowerConsumption(binary: List[String]): Int ={
    val gammaRate = calculateGammaRate(binary)
    val epsilonRate = invertBinary(gammaRate)

    val gammaInt = Integer.parseInt(gammaRate, 2)
    val epsilonInt = Integer.parseInt(epsilonRate, 2)

    gammaInt * epsilonInt
  }

  @tailrec
  def getRating(binary: List[String], bitCriteria: (List[String], Int) => Char, index: Int): String = {
    binary match {
      case x :: Nil => x
      case _ =>
        val bit = bitCriteria(binary, index)
        val matchingBinary = binary.filter(_.charAt(index) == bit)
        getRating(matchingBinary, bitCriteria, index + 1)
    }
  }

  def calculateLifeSupportRating(binary: List[String]): Int = {
    val oxygenRating = getRating(binary, mostPopularBitAtIndex, 0)
    val co2Rating = getRating(binary, leastPopularBitAtIndex, 0)

    val oxygenInt = Integer.parseInt(oxygenRating, 2)
    val co2Int = Integer.parseInt(co2Rating, 2)

    oxygenInt * co2Int
  }

  val binary = Utils.readFileAsListOfString("day-3-part-1.txt")
  val powerConsumption = calculatePowerConsumption(binary)
  println(s"Power Consumption: $powerConsumption")
  val lifeSupportRating = calculateLifeSupportRating(binary)
  println(s"Life Support Rating: $lifeSupportRating")

}
