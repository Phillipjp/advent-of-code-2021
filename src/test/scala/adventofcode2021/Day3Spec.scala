package adventofcode2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day3Spec extends AnyWordSpec with Matchers {

  "calculateGammaRate" should {
    "calculate the gamma rate from a list of binary numbers" in {
      // Given
      val binary = List("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
      // When
      val actual = Day3.calculateGammaRate(binary)
      // Then
      actual shouldBe "10110"
    }
  }

  "invertBinary" should {
    "flip the bits in a binary number" in {
      Day3.invertBinary("10110") shouldBe "01001"
    }
  }

  "calculatePowerConsumption" should {
    "get the gamma and epsilon rates then calculate the product" in {
      // Given
      val binary = List("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
      // When
      val actual = Day3.calculatePowerConsumption(binary)
      // Then
      actual shouldBe 198
    }
  }

  "getRating" should {
    "get the only binary number that matches the criteria for the most popular bit" in {
      // Given
      val binary = List("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
      // When
      val actual = Day3.getRating(binary, Day3.mostPopularBitAtIndex, 0)
      // Then
      actual shouldBe "10111"
    }

    "get the only binary number that matches the criteria for the least popular bit" in {
      // Given
      val binary = List("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
      // When
      val actual = Day3.getRating(binary, Day3.leastPopularBitAtIndex, 0)
      // Then
      actual shouldBe "01010"
    }
  }

  "calculateLifeSupportRating" should {
    "get the oxygen and co2 ratings then calculate the product" in {
      // Given
      val binary = List("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
      // When
      val actual = Day3.calculateLifeSupportRating(binary)
      // Then
      actual shouldBe 230
    }

  }
}
