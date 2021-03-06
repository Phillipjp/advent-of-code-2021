package adventofcode2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day5Spec extends AnyWordSpec with Matchers {

  "parseInput" should {
    "make a list of lines" in {
      // Given
      val input = List(
      "0,9 -> 5,9",
      "8,0 -> 0,8",
      "9,4 -> 3,4",
      "2,2 -> 2,1",
      "7,0 -> 7,4",
      "6,4 -> 2,0",
      "0,9 -> 2,9",
      "3,4 -> 1,4",
      "0,0 -> 8,8",
      "5,5 -> 8,2"
      )
      val expected = List(Line(Coord(0,9),Coord(5,9)), Line(Coord(8,0),Coord(0,8)), Line(Coord(9,4),Coord(3,4)), Line(Coord(2,2),Coord(2,1)), Line(Coord(7,0),Coord(7,4)), Line(Coord(6,4),Coord(2,0)), Line(Coord(0,9),Coord(2,9)), Line(Coord(3,4),Coord(1,4)), Line(Coord(0,0),Coord(8,8)), Line(Coord(5,5),Coord(8,2)))

      // When
      val actual = Day5.parseInput(input)

      // Then
      actual shouldBe expected
    }
  }

  "mapVents" should {

    "map each coordinate to the number of vents there with horizontal and vertical  vents" in {
      // Given
      val lines = List(Line(Coord(0,9),Coord(5,9)), Line(Coord(9,4),Coord(3,4)), Line(Coord(2,2),Coord(2,1)), Line(Coord(7,0),Coord(7,4)), Line(Coord(0,9),Coord(2,9)), Line(Coord(3,4),Coord(1,4)))
      val expected = Map((7,1) -> 1, (3,9) -> 1, (7,4) -> 2, (3,4) -> 2, (6,4) -> 1, (0,9) -> 2, (5,9) -> 1, (4,4) -> 1, (7,3) -> 1, (1,9) -> 2, (1,4) -> 1, (4,9) -> 1, (2,9) -> 2, (5,4) -> 1, (2,2) -> 1, (2,4) -> 1, (8,4) -> 1, (2,1) -> 1, (9,4) -> 1, (7,2) -> 1, (7,0) -> 1)
      // When
      val actual = Day5.mapVents(lines)

      // Then
      actual shouldBe expected

    }

    "map each coordinate to the number of vents there with horizontal, vertical and diagonal vents" in {
      // Given
      val lines = List(Line(Coord(0,9),Coord(5,9)), Line(Coord(8,0),Coord(0,8)), Line(Coord(9,4),Coord(3,4)), Line(Coord(2,2),Coord(2,1)), Line(Coord(7,0),Coord(7,4)), Line(Coord(6,4),Coord(2,0)), Line(Coord(0,9),Coord(2,9)), Line(Coord(3,4),Coord(1,4)), Line(Coord(0,0),Coord(8,8)), Line(Coord(5,5),Coord(8,2)))
      val expected = Map((7,1) -> 2, (3,9) -> 1, (0,0) -> 1, (7,4) -> 2, (3,4) -> 2, (6,4) -> 3, (7,7) -> 1, (0,9) -> 2, (6,6) -> 1, (3,1) -> 1, (5,9) -> 1, (6,2) -> 1, (2,0) -> 1, (4,4) -> 3, (8,0) -> 1, (1,1) -> 1, (3,5) -> 1, (7,3) -> 2, (1,9) -> 2, (1,4) -> 1, (2,6) -> 1, (8,2) -> 1, (0,8) -> 1, (8,8) -> 1, (4,9) -> 1, (2,9) -> 2, (5,4) -> 1, (2,2) -> 2, (5,5) -> 2, (4,2) -> 1, (2,4) -> 1, (8,4) -> 1, (5,3) -> 2, (3,3) -> 1, (1,7) -> 1, (2,1) -> 1, (9,4) -> 1, (7,2) -> 1, (7,0) -> 1)

      // When
      val actual = Day5.mapVents(lines)

      // Then
      actual shouldBe expected

    }
  }

}
