package adventofcode2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day2Spec extends AnyWordSpec with Matchers {

  "part one" should {
    "get the end coordinates of the submarine" in {
      //Given
      val instructions = List(
        ("forward", 5),
        ("down", 5),
        ("forward", 8),
        ("up", 3),
        ("down", 8),
        ("forward", 2)
      )
      //When
      val actual = Day2.followInstructions(instructions, (0,0), Day2.completePartOneInstruction)
      //Then
      actual shouldBe (15,10)
    }
  }
  "part two" should {
    "get the end coordinates and aim of the submarine" in {
      //Given
      val instructions = List(
        ("forward", 5),
        ("down", 5),
        ("forward", 8),
        ("up", 3),
        ("down", 8),
        ("forward", 2)
      )
      //When
      val actual = Day2.followInstructions(instructions, (0,0,0), Day2.completePartTwoInstruction)
      //Then
      actual shouldBe (15,60,10)
    }
  }

}
