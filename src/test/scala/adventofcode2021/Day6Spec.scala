package adventofcode2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Spec extends AnyWordSpec with Matchers {

  "simulateFishBreeding" should {
    "get a list of all the fish breeding times after 18 days" in {
      // Given
      val fish = List(3,4,3,1,2)
      val expected = List(6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8)
      // When
      val actual = Day6.simulateFishBreeding(fish, 18)
      actual shouldBe expected
    }
  }

  "simulateFishBreedingV2" should {
    "number of fish after 18 days" in {
      // Given
      val fish = List(3,4,3,1,2)
      // When
      val actual = Day6.simulateFishBreedingV2(fish, 18)
      //Then
      actual.values.sum shouldBe 26
    }
  }

}
