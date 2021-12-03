package adventofcode2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day1Spec extends AnyWordSpec with Matchers {

  "part one" should {
    "count the correct number of increases" in {
      // Given
      val depths = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
      // When
      val actual = Day1.partOne(depths)
      //Then
      actual shouldBe 7
    }
  }

  "part two" should {
    "count the correct number of increases" in {
      // Given
      val depths = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
      // When
      val actual = Day1.partTwo(depths)
      //Then
      actual shouldBe 5
    }
  }

}
