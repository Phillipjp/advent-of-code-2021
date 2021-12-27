package adventofcode2021

import scala.annotation.tailrec

object Day11 extends App{

  val input = Utils.readFileAsListOfString("day-11.txt")
  val octopusEnergyMap = makeOctopusEnergyMap(input)
  val answer1 = numberOfFlashesAfterNSteps(octopusEnergyMap, 100)
  println(answer1)
  val answer2 = findWhenOctopusesFlashesSync(octopusEnergyMap)
  println(answer2)


  def numberOfFlashesAfterNSteps(octopusEnergyMap: Map[(Int, Int), Int], n: Int): Int = {


    @tailrec
    def turn(octopusEnergyMap: Map[(Int, Int), Int], n: Int, i: Int, flashes: Int): Int = {
      if(n == i)
        flashes
      else{
        val updatedOctopusEnergyMap = increaseAllOctopusEnergies(octopusEnergyMap)
        val octopusesToFlash = getOctopusesToFlash(updatedOctopusEnergyMap).keySet.toList

        val flashedOctopuses = flashOctopuses(octopusesToFlash, updatedOctopusEnergyMap)

        val turnFlashes = getOctopusesToFlash(flashedOctopuses).size
        val resetOctopusEnergyMap = resetOctopusEnergies(flashedOctopuses)

        turn(resetOctopusEnergyMap, n, i+1, flashes + turnFlashes)
      }
    }

    turn(octopusEnergyMap, n, 0, 0)
  }

  def findWhenOctopusesFlashesSync(octopusEnergyMap: Map[(Int, Int), Int]): Int = {
    @tailrec
    def turn(octopusEnergyMap: Map[(Int, Int), Int], i: Int): Int = {
      if(octopusEnergyMap.values.forall(_ == 0))
        i
      else{
        val updatedOctopusEnergyMap = increaseAllOctopusEnergies(octopusEnergyMap)
        val octopusesToFlash = getOctopusesToFlash(updatedOctopusEnergyMap).keySet.toList

        val flashedOctopuses = flashOctopuses(octopusesToFlash, updatedOctopusEnergyMap)
        val resetOctopusEnergyMap = resetOctopusEnergies(flashedOctopuses)

        turn(resetOctopusEnergyMap,  i+1)
      }
    }

    turn(octopusEnergyMap, 0)
  }

  @tailrec
  def flashOctopuses(octopusesToFlash: List[(Int, Int)], octopusEnergyMap: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    octopusesToFlash match {
      case Nil => octopusEnergyMap
      case (x, y) :: tail =>
        val updatedOctopusEnergyMap = updateSurroundingValues(octopusEnergyMap, x, y)
        val newlyFlashedSurroundingOctopuses = getSurroundingNewlyFlashedOctopuses(updatedOctopusEnergyMap, x, y)
        flashOctopuses((tail ++ newlyFlashedSurroundingOctopuses).distinct, updatedOctopusEnergyMap)
    }
  }

  def getSurroundingNewlyFlashedOctopuses(octopusEnergyMap: Map[(Int, Int), Int], x: Int, y: Int): List[(Int, Int)] = {
    getSurroundingCoords(x, y).flatMap { c =>
      octopusEnergyMap.get(c) match {
        case Some(v) =>
          if(v == 10) Some(c) else None
        case _ => None
      }
    }
  }

  def resetOctopusEnergies(octopusEnergyMap: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
      octopusEnergyMap.map{case ((x,y), energy) => if(energy > 9) (x,y) -> 0 else (x,y) -> energy}
  }

  def getSurroundingCoords(x: Int, y: Int): List[(Int, Int)] = {
    List((x-1, y+1), (x, y+1), (x+1, y+1), (x+1, y), (x+1, y-1), (x, y-1), (x-1, y-1), (x-1, y))
  }

  def updateSurroundingValues(octopusEnergyMap: Map[(Int, Int), Int], x: Int, y: Int): Map[(Int, Int), Int] = {
    val surroundingCoords = getSurroundingCoords(x, y)
    surroundingCoords.foldLeft(octopusEnergyMap){case (octopusEnergyMap, (x,y)) =>
      octopusEnergyMap.get((x,y)) match {
        case Some(energy) => octopusEnergyMap + ((x,y) -> (energy + 1))
        case None => octopusEnergyMap
      }
    }
  }

  def increaseAllOctopusEnergies(octopusEnergyMap: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    octopusEnergyMap.map{case((x,y), octopusEnergy) => (x,y) -> (octopusEnergy + 1)}
  }

  def getOctopusesToFlash(octopusEnergyMap: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    octopusEnergyMap.filter{case((_,_), octopusEnergy) => octopusEnergy > 9}
  }

  def makeOctopusEnergyMap(input: List[String]): Map[(Int, Int), Int] =
    input.zipWithIndex
      .flatMap { case (line, y) =>
        line.zipWithIndex.map { case (octopusEnergy, x) => (x, y) -> octopusEnergy.toString.toInt }
      }
      .toMap


  def printMap(map: Map[(Int,Int), Int], x: Int, y: Int): Unit = {
    for(i <- 0 until  y){
      for(j <- 0 until x){
        print(map(j,i))
      }
      println()
    }
  }
}
