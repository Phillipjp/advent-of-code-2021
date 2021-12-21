package adventofcode2021

import scala.annotation.tailrec

object Day6 extends App{

  val input = Utils.readFileAsListOfString("day-6.txt").head.split(",").toList.map(_.toInt)
  val partOneAnswer = simulateFishBreedingV2(input, 80).values.sum
  println(partOneAnswer)
  val partTwoAnswer = simulateFishBreedingV2(input, 256).values.sum
  println(partTwoAnswer)

  def simulateFishBreeding(fish: List[Int], numDays: Int): List[Int] = {
    @tailrec
    def simulate(fish: List[Int], day: Int , numDays: Int): List[Int] = {
      if(day == numDays) {
        fish
      }
      else{
        val newFish = Stream.continually(8).take(fish.count(_ == 0))
        val fishTimers = fish.map{i =>
          if(i == 0) 6
          else i-1
        }

        simulate(fishTimers ++ newFish, day + 1, numDays)
      }
    }

    simulate(fish, 0, numDays)
  }

  def simulateFishBreedingV2(fish: List[Int], numDays: Int): Map[Int, Long] = {
    @tailrec
    def simulate(groupedFish: Map[Int, Long], day: Int, numDays: Int): Map[Int, Long] = {
      if (day == numDays) {
        groupedFish
      }
      else {
        val updatedGroupedFish = groupedFish.map { case (day, count) =>
          if(day == 0)
            6 -> count
          else
          (day - 1) -> count
        }

        simulate(updatedGroupedFish ++ Map(8 -> groupedFish(0), 6 -> (groupedFish(0) + updatedGroupedFish(6))), day + 1, numDays)
      }
    }

    val groupedFish = Map(
      0 -> fish.count(_ == 0).toLong,
      1 -> fish.count(_ == 1).toLong,
      2 -> fish.count(_ == 2).toLong,
      3 -> fish.count(_ == 3).toLong,
      4 -> fish.count(_ == 4).toLong,
      5 -> fish.count(_ == 5).toLong,
      6 -> fish.count(_ == 6).toLong,
      7 -> fish.count(_ == 7).toLong,
      8 -> fish.count(_ == 8).toLong
    )
    simulate(groupedFish, 0, numDays)
  }

}
