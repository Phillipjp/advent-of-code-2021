package adventofcode2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day11Spec extends AnyWordSpec with Matchers {

  "increaseAllOctopusEnergies" should{
    "increase all values in map by 1" in {
      // Given
      val input = Map((0,0) -> 0, (1,1) -> 1, (2,2) -> 2)
      val expected = Map((0,0) -> 1, (1,1) -> 2, (2,2) -> 3)

      // When
      val actual = Day11.increaseAllOctopusEnergies(input)

      // Then
      actual shouldBe expected
    }
  }

  "getOctopusesToFlash" should {
    "get all the entries that have a value greater than 9" in {
      // Given
      val input = Map((0,0) -> 9, (1,1) -> 10, (2,2) -> 11, (3,3) -> 8)
      val expected = Map((1,1) -> 10, (2,2) -> 11)

      // When
      val actual = Day11.getOctopusesToFlash(input)

      // Then
      actual shouldBe expected
    }
  }

  "updateSurroundingValues" should {
    "update all the surrounding values when all the surrounding values exist" in {
      // Given
      val input = Map((0,2) -> 0, (1,2) -> 0, (2,2) -> 0, (2,1) -> 0, (2,0) -> 0, (1,0) -> 0, (0,0) -> 0, (0,1) -> 0, (1,1) -> 1)
      val expected = Map((0,2) -> 1, (1,2) -> 1, (2,2) -> 1, (2,1) -> 1, (2,0) -> 1, (1,0) -> 1, (0,0) -> 1, (0,1) -> 1, (1,1) -> 1)

      // When
      val actual = Day11.updateSurroundingValues(input, 1, 1)

      // Then
      actual shouldBe expected
    }

    "update all the surrounding values when not all the surrounding values exist" in {
      // Given
      val input = Map((0,2) -> 0, (1,2) -> 0, (2,2) -> 0, (2,1) -> 0, (2,0) -> 0, (1,0) -> 0, (0,0) -> 0, (0,1) -> 1, (1,1) -> 0)
      val expected = Map((0,2) -> 1, (1,2) -> 1, (2,2) -> 0, (2,1) -> 0, (2,0) -> 0, (1,0) -> 1, (0,0) -> 1, (0,1) -> 1, (1,1) -> 1)

      // When
      val actual = Day11.updateSurroundingValues(input, 0, 1)

      // Then
      actual shouldBe expected
    }
  }

  "resetOctopusEnergies" should {
    "reset all the entries that have a value greater than 9 to 0" in {
      // Given
      val input = Map((0,0) -> 9, (1,1) -> 10, (2,2) -> 11, (3,3) -> 8)
      val expected = Map((0,0) -> 9, (1,1) -> 0, (2,2) -> 0, (3,3) -> 8)

      // When
      val actual = Day11.resetOctopusEnergies(input)

      // Then
      actual shouldBe expected
    }
  }

}
