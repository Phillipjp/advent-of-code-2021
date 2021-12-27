package adventofcode2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day14Spec extends AnyWordSpec with Matchers {

  "growPolymer" should {
    "grow a polymer for 4 turns" in {
      // Given
      val rules = Map("CH" -> "B", "HH" -> "N", "CB" -> "H", "NH" -> "C", "HB" -> "C", "HC" -> "B", "HN" -> "C", "NN" -> "C", "BH" -> "H", "NC" -> "B", "NB" -> "B", "BN" -> "B", "BB" -> "N", "BC" -> "B", "CC" -> "N", "CN" -> "C")
      val template = "NNCB"
      val expected = "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
      // When
      val actual = Day14.growPolymer(template, rules, 4)
      // Then
      println()
      println(expected)
      println(actual)
      actual shouldBe expected
    }
  }
  "mostCommonLetter" should {
    "find the most common letter in a string" in {
      //Given
      val s = "abcb"
      // When
      val actual: Char = Day14.mostCommonLetter(s)
      // Then
      actual shouldBe 'b'
    }
  }

  "leastCommonLetter" should {
    "find the least common letter in a string" in {
      //Given
      val s = "abcbc"
      // When
      val actual: Char = Day14.leastCommonLetter(s)
      // Then
      actual shouldBe 'a'
    }
  }

}
