package adventofcode2021

import scala.annotation.tailrec
import scalaz.Scalaz._

object Day12 extends App {

  val input = Utils.readFileAsListOfString("day-12.txt")
  val caveConnections = createConnections(input)
  val allRoutes = findAllRoutes(caveConnections)
  println(allRoutes.size)
  val allRoutesPart2 = findAllRoutesPart2(caveConnections)
  println(allRoutesPart2.size)

  def createConnections(input: List[String]): Map[String, Set[String]] = {

    @tailrec
    def parseInput(input: List[String], connections: Map[String, Set[String]]): Map[String, Set[String]] = {
      input match {
        case Nil => connections
        case x :: tail =>
          val (c1, c2) = (x.split("-").head, x.split("-").last)
          val map1 = updateMap(c1, c2, connections)
          val map2 = updateMap(c2, c1, map1)
          parseInput(tail, map2)
      }
    }

    parseInput(input, Map())
  }

  def updateMap(key: String, value: String, map: Map[String, Set[String]]): Map[String, Set[String]] = {
    map.get(key) match {
      case None => map + (key -> Set(value))
      case Some(values) => map + (key -> (values + value))
    }
  }

  def findAllRoutes(caveConnections: Map[String, Set[String]]): List[List[String]] = {

    @tailrec
    def traverseCaves(routes: List[List[String]], caveConnections: Map[String, Set[String]]): List[List[String]] = {
      if (routes.forall(_.last == "end"))
        routes
      else {
        val endedRoutes = routes.filter(_.last == "end")
        val inProgressRoutes = routes.filterNot(_.last == "end")
        val updatedInpProgressRoutes = inProgressRoutes.flatMap { route =>
          val nextCaves = caveConnections(route.last)
          val updatedRoutes = nextCaves.toList.flatMap { cave =>
            if (cave.forall(_.isLower) && route.contains(cave))
              None
            else
              Some(route ++ Seq(cave))
          }
          updatedRoutes
        }
        traverseCaves(endedRoutes ++ updatedInpProgressRoutes, caveConnections)
      }
    }

    traverseCaves(List(List("start")), caveConnections)
  }

  def findAllRoutesPart2(caveConnections: Map[String, Set[String]]): List[List[String]] = {

    @tailrec
    def traverseCaves(routes: List[List[String]], caveConnections: Map[String, Set[String]]): List[List[String]] = {
      if (routes.forall(_.last == "end"))
        routes
      else {
        val endedRoutes = routes.filter(_.last == "end")
        val inProgressRoutes = routes.filterNot(_.last == "end")
        val updatedInpProgressRoutes = inProgressRoutes.flatMap { route =>
          val nextCaves = caveConnections(route.last)

          val updatedRoutes = nextCaves.toList.flatMap { cave =>
            val smallCavesCount = route.filter(_.forall(_.isLower)).groupBy(c => c).mapValues(_.size) |+| Map(cave -> 1)
            val revisitedSmallCave = smallCavesCount.values.exists(_ == 3) || smallCavesCount.values.count(_ == 2) > 1
            if(cave == "start" && route.count(_ == "start") == 1)
              None
            else if (cave.forall(_.isLower) && revisitedSmallCave)
              None
            else
              Some(route ++ Seq(cave))
          }
          updatedRoutes
        }
        traverseCaves(endedRoutes ++ updatedInpProgressRoutes, caveConnections)
      }
    }

    traverseCaves(List(List("start")), caveConnections)
  }

}
