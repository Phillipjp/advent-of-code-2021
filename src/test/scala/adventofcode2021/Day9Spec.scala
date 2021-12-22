package adventofcode2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day9Spec extends AnyWordSpec with Matchers {

  "makeHeightMap" should {

    "make a map of the heights" in {
      // Given
      val input = List(
      "2199943210",
      "3987894921",
      "9856789892",
      "8767896789",
      "9899965678"
      )



      // When
      val actual = Day9.makeHeightMap(input)

      // Then
      actual shouldBe Map((7,1) -> 9, (5,0) -> 4, (9,0) -> 0, (0,2) -> 9, (0,0) -> 2, (5,2) -> 8, (7,4) -> 6, (5,1) -> 9, (4,0) -> 9, (3,4) -> 9, (6,4) -> 5, (3,1) -> 7, (9,1) -> 1, (6,1) -> 4, (4,1) -> 8, (6,2) -> 9, (8,1) -> 2, (2,0) -> 9, (0,3) -> 8, (4,4) -> 9, (3,0) -> 9, (8,0) -> 1, (1,1) -> 9, (6,3) -> 6, (7,3) -> 7, (8,3) -> 8, (1,4) -> 8, (8,2) -> 9, (0,4) -> 9, (5,4) -> 6, (3,2) -> 6, (1,3) -> 7, (2,2) -> 5, (4,2) -> 7, (2,4) -> 9, (0,1) -> 3, (8,4) -> 7, (5,3) -> 9, (3,3) -> 7, (2,3) -> 6, (1,2) -> 8, (2,1) -> 8, (4,3) -> 8, (6,0) -> 3, (9,3) -> 9, (9,4) -> 8, (7,2) -> 8, (1,0) -> 1, (9,2) -> 2, (7,0) -> 2)

    }

  }

  "findLowPoints" should {
    "find all the low points in the height map" in {
      // Given
      val heightMap = Map((7,1) -> 9, (5,0) -> 4, (9,0) -> 0, (0,2) -> 9, (0,0) -> 2, (5,2) -> 8, (7,4) -> 6, (5,1) -> 9, (4,0) -> 9, (3,4) -> 9, (6,4) -> 5, (3,1) -> 7, (9,1) -> 1, (6,1) -> 4, (4,1) -> 8, (6,2) -> 9, (8,1) -> 2, (2,0) -> 9, (0,3) -> 8, (4,4) -> 9, (3,0) -> 9, (8,0) -> 1, (1,1) -> 9, (6,3) -> 6, (7,3) -> 7, (8,3) -> 8, (1,4) -> 8, (8,2) -> 9, (0,4) -> 9, (5,4) -> 6, (3,2) -> 6, (1,3) -> 7, (2,2) -> 5, (4,2) -> 7, (2,4) -> 9, (0,1) -> 3, (8,4) -> 7, (5,3) -> 9, (3,3) -> 7, (2,3) -> 6, (1,2) -> 8, (2,1) -> 8, (4,3) -> 8, (6,0) -> 3, (9,3) -> 9, (9,4) -> 8, (7,2) -> 8, (1,0) -> 1, (9,2) -> 2, (7,0) -> 2)
      val expected = Seq(1,0,5,5)
      // When
      val actual = Day9.findLowPoints(heightMap)
      // Then
      actual should contain theSameElementsAs  expected
    }
  }

  "getBasin" should{
    "get the size of basin given a low point" in {
      // Given
      val heightMap = Map((7,1) -> 9, (5,0) -> 4, (9,0) -> 0, (0,2) -> 9, (0,0) -> 2, (5,2) -> 8, (7,4) -> 6, (5,1) -> 9, (4,0) -> 9, (3,4) -> 9, (6,4) -> 5, (3,1) -> 7, (9,1) -> 1, (6,1) -> 4, (4,1) -> 8, (6,2) -> 9, (8,1) -> 2, (2,0) -> 9, (0,3) -> 8, (4,4) -> 9, (3,0) -> 9, (8,0) -> 1, (1,1) -> 9, (6,3) -> 6, (7,3) -> 7, (8,3) -> 8, (1,4) -> 8, (8,2) -> 9, (0,4) -> 9, (5,4) -> 6, (3,2) -> 6, (1,3) -> 7, (2,2) -> 5, (4,2) -> 7, (2,4) -> 9, (0,1) -> 3, (8,4) -> 7, (5,3) -> 9, (3,3) -> 7, (2,3) -> 6, (1,2) -> 8, (2,1) -> 8, (4,3) -> 8, (6,0) -> 3, (9,3) -> 9, (9,4) -> 8, (7,2) -> 8, (1,0) -> 1, (9,2) -> 2, (7,0) -> 2)

      // When
      val actual = Day9.getBasin(heightMap, 9, 0)

      // Then
      actual shouldBe 9
    }
  }

  "findBasins" should {
    "find the sizes of all the basins in a height map" in {
      // Given
      val heightMap = Map((7,1) -> 9, (5,0) -> 4, (9,0) -> 0, (0,2) -> 9, (0,0) -> 2, (5,2) -> 8, (7,4) -> 6, (5,1) -> 9, (4,0) -> 9, (3,4) -> 9, (6,4) -> 5, (3,1) -> 7, (9,1) -> 1, (6,1) -> 4, (4,1) -> 8, (6,2) -> 9, (8,1) -> 2, (2,0) -> 9, (0,3) -> 8, (4,4) -> 9, (3,0) -> 9, (8,0) -> 1, (1,1) -> 9, (6,3) -> 6, (7,3) -> 7, (8,3) -> 8, (1,4) -> 8, (8,2) -> 9, (0,4) -> 9, (5,4) -> 6, (3,2) -> 6, (1,3) -> 7, (2,2) -> 5, (4,2) -> 7, (2,4) -> 9, (0,1) -> 3, (8,4) -> 7, (5,3) -> 9, (3,3) -> 7, (2,3) -> 6, (1,2) -> 8, (2,1) -> 8, (4,3) -> 8, (6,0) -> 3, (9,3) -> 9, (9,4) -> 8, (7,2) -> 8, (1,0) -> 1, (9,2) -> 2, (7,0) -> 2)

      // When
      val actual = Day9.findBasins(heightMap)

      // Then
      actual should contain theSameElementsAs Seq(3, 9, 14, 9)
    }
  }

}
