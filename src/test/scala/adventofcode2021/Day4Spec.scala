package adventofcode2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day4Spec extends AnyWordSpec with Matchers {

  "parseBingoCards" should {
    "make bingo cards" in {
      // Given
      val input = List (
        "22 13 17 11  0",
        "8  2 23  4 24",
        "21  9 14 16  7",
        "6 10  3 18  5",
        "1 12 20 15 19",
        "",
        "3 15  0  2 22",
        "9 18 13 17  5",
        "19  8  7 25 23",
        "20 11 10 24  4",
        "14 21 16 12  6",
        "",
        "14 21 17 24  4",
        "10 16 15  9 19",
        "18  8 23 26 20",
        "22 11 13  6  5",
        "2  0 12  3  7"
      )

      val expected = List(
          Map((0,2) -> BingoCardNumber(21,false), (0,0) -> BingoCardNumber(22,false), (4,0) -> BingoCardNumber(0,false), (3,4) -> BingoCardNumber(15,false), (3,1) -> BingoCardNumber(4,false), (4,1) -> BingoCardNumber(24,false), (2,0) -> BingoCardNumber(17,false), (0,3) -> BingoCardNumber(6,false), (4,4) -> BingoCardNumber(19,false), (3,0) -> BingoCardNumber(11,false), (1,1) -> BingoCardNumber(2,false), (1,4) -> BingoCardNumber(12,false), (0,4) -> BingoCardNumber(1,false), (3,2) -> BingoCardNumber(16,false), (1,3) -> BingoCardNumber(10,false), (2,2) -> BingoCardNumber(14,false), (4,2) -> BingoCardNumber(7,false), (2,4) -> BingoCardNumber(20,false), (0,1) -> BingoCardNumber(8,false), (3,3) -> BingoCardNumber(18,false), (2,3) -> BingoCardNumber(3,false), (1,2) -> BingoCardNumber(9,false), (2,1) -> BingoCardNumber(23,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(13,false)),
          Map((0,2) -> BingoCardNumber(19,false), (0,0) -> BingoCardNumber(3,false), (4,0) -> BingoCardNumber(22,false), (3,4) -> BingoCardNumber(12,false), (3,1) -> BingoCardNumber(17,false), (4,1) -> BingoCardNumber(5,false), (2,0) -> BingoCardNumber(0,false), (0,3) -> BingoCardNumber(20,false), (4,4) -> BingoCardNumber(6,false), (3,0) -> BingoCardNumber(2,false), (1,1) -> BingoCardNumber(18,false), (1,4) -> BingoCardNumber(21,false), (0,4) -> BingoCardNumber(14,false), (3,2) -> BingoCardNumber(25,false), (1,3) -> BingoCardNumber(11,false), (2,2) -> BingoCardNumber(7,false), (4,2) -> BingoCardNumber(23,false), (2,4) -> BingoCardNumber(16,false), (0,1) -> BingoCardNumber(9,false), (3,3) -> BingoCardNumber(24,false), (2,3) -> BingoCardNumber(10,false), (1,2) -> BingoCardNumber(8,false), (2,1) -> BingoCardNumber(13,false), (4,3) -> BingoCardNumber(4,false), (1,0) -> BingoCardNumber(15,false)),
          Map((0,2) -> BingoCardNumber(18,false), (0,0) -> BingoCardNumber(14,false), (4,0) -> BingoCardNumber(4,false), (3,4) -> BingoCardNumber(3,false), (3,1) -> BingoCardNumber(9,false), (4,1) -> BingoCardNumber(19,false), (2,0) -> BingoCardNumber(17,false), (0,3) -> BingoCardNumber(22,false), (4,4) -> BingoCardNumber(7,false), (3,0) -> BingoCardNumber(24,false), (1,1) -> BingoCardNumber(16,false), (1,4) -> BingoCardNumber(0,false), (0,4) -> BingoCardNumber(2,false), (3,2) -> BingoCardNumber(26,false), (1,3) -> BingoCardNumber(11,false), (2,2) -> BingoCardNumber(23,false), (4,2) -> BingoCardNumber(20,false), (2,4) -> BingoCardNumber(12,false), (0,1) -> BingoCardNumber(10,false), (3,3) -> BingoCardNumber(6,false), (2,3) -> BingoCardNumber(13,false), (1,2) -> BingoCardNumber(8,false), (2,1) -> BingoCardNumber(15,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(21,false))

      )

      // When
      val actual = Day4.parseBingoCards(input)

      // Then
      actual shouldBe expected
    }
  }

  "markCard" should {
    "mark the correct number on a bingo card" in {
      // Given
      val card = Map((0,2) -> BingoCardNumber(21,false), (0,0) -> BingoCardNumber(22,false), (4,0) -> BingoCardNumber(0,false), (3,4) -> BingoCardNumber(15,false), (3,1) -> BingoCardNumber(4,false), (4,1) -> BingoCardNumber(24,false), (2,0) -> BingoCardNumber(17,false), (0,3) -> BingoCardNumber(6,false), (4,4) -> BingoCardNumber(19,false), (3,0) -> BingoCardNumber(11,false), (1,1) -> BingoCardNumber(2,false), (1,4) -> BingoCardNumber(12,false), (0,4) -> BingoCardNumber(1,false), (3,2) -> BingoCardNumber(16,false), (1,3) -> BingoCardNumber(10,false), (2,2) -> BingoCardNumber(14,false), (4,2) -> BingoCardNumber(7,false), (2,4) -> BingoCardNumber(20,false), (0,1) -> BingoCardNumber(8,false), (3,3) -> BingoCardNumber(18,false), (2,3) -> BingoCardNumber(3,false), (1,2) -> BingoCardNumber(9,false), (2,1) -> BingoCardNumber(23,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(13,false))
      val expected = Map((0,2) -> BingoCardNumber(21,true), (0,0) -> BingoCardNumber(22,false), (4,0) -> BingoCardNumber(0,false), (3,4) -> BingoCardNumber(15,false), (3,1) -> BingoCardNumber(4,false), (4,1) -> BingoCardNumber(24,false), (2,0) -> BingoCardNumber(17,false), (0,3) -> BingoCardNumber(6,false), (4,4) -> BingoCardNumber(19,false), (3,0) -> BingoCardNumber(11,false), (1,1) -> BingoCardNumber(2,false), (1,4) -> BingoCardNumber(12,false), (0,4) -> BingoCardNumber(1,false), (3,2) -> BingoCardNumber(16,false), (1,3) -> BingoCardNumber(10,false), (2,2) -> BingoCardNumber(14,false), (4,2) -> BingoCardNumber(7,false), (2,4) -> BingoCardNumber(20,false), (0,1) -> BingoCardNumber(8,false), (3,3) -> BingoCardNumber(18,false), (2,3) -> BingoCardNumber(3,false), (1,2) -> BingoCardNumber(9,false), (2,1) -> BingoCardNumber(23,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(13,false))

      // When
      val actual = Day4.markCard(card, 21)

      // Then
      actual shouldBe expected
    }

    "don't update the card if it doesn't contain the number" in {
      // Given
      val card = Map((0,2) -> BingoCardNumber(21,false), (0,0) -> BingoCardNumber(22,false), (4,0) -> BingoCardNumber(0,false), (3,4) -> BingoCardNumber(15,false), (3,1) -> BingoCardNumber(4,false), (4,1) -> BingoCardNumber(24,false), (2,0) -> BingoCardNumber(17,false), (0,3) -> BingoCardNumber(6,false), (4,4) -> BingoCardNumber(19,false), (3,0) -> BingoCardNumber(11,false), (1,1) -> BingoCardNumber(2,false), (1,4) -> BingoCardNumber(12,false), (0,4) -> BingoCardNumber(1,false), (3,2) -> BingoCardNumber(16,false), (1,3) -> BingoCardNumber(10,false), (2,2) -> BingoCardNumber(14,false), (4,2) -> BingoCardNumber(7,false), (2,4) -> BingoCardNumber(20,false), (0,1) -> BingoCardNumber(8,false), (3,3) -> BingoCardNumber(18,false), (2,3) -> BingoCardNumber(3,false), (1,2) -> BingoCardNumber(9,false), (2,1) -> BingoCardNumber(23,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(13,false))

      // When
      val actual = Day4.markCard(card, 100)

      // Then
      actual shouldBe card
    }
  }

  "checkForBingo" should {
    "return a card if it contain a fully marked row" in {
      // Given
      val card = Map((0,2) -> BingoCardNumber(21,false), (0,0) -> BingoCardNumber(22,true), (4,0) -> BingoCardNumber(0,true), (3,4) -> BingoCardNumber(15,false), (3,1) -> BingoCardNumber(4,false), (4,1) -> BingoCardNumber(24,false), (2,0) -> BingoCardNumber(17,true), (0,3) -> BingoCardNumber(6,false), (4,4) -> BingoCardNumber(19,false), (3,0) -> BingoCardNumber(11,true), (1,1) -> BingoCardNumber(2,false), (1,4) -> BingoCardNumber(12,false), (0,4) -> BingoCardNumber(1,false), (3,2) -> BingoCardNumber(16,false), (1,3) -> BingoCardNumber(10,false), (2,2) -> BingoCardNumber(14,false), (4,2) -> BingoCardNumber(7,false), (2,4) -> BingoCardNumber(20,false), (0,1) -> BingoCardNumber(8,false), (3,3) -> BingoCardNumber(18,false), (2,3) -> BingoCardNumber(3,false), (1,2) -> BingoCardNumber(9,false), (2,1) -> BingoCardNumber(23,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(13,true))

      // When
      val actual = Day4.checkForBingo(card).get

      // Then
      actual shouldBe card
    }

    "return a card if it contain a fully marked column" in {
      // Given
      val card = Map((0,2) -> BingoCardNumber(21,true), (0,0) -> BingoCardNumber(22,true), (4,0) -> BingoCardNumber(0,false), (3,4) -> BingoCardNumber(15,false), (3,1) -> BingoCardNumber(4,false), (4,1) -> BingoCardNumber(24,false), (2,0) -> BingoCardNumber(17,false), (0,3) -> BingoCardNumber(6,true), (4,4) -> BingoCardNumber(19,false), (3,0) -> BingoCardNumber(11,false), (1,1) -> BingoCardNumber(2,false), (1,4) -> BingoCardNumber(12,false), (0,4) -> BingoCardNumber(1,true), (3,2) -> BingoCardNumber(16,false), (1,3) -> BingoCardNumber(10,false), (2,2) -> BingoCardNumber(14,false), (4,2) -> BingoCardNumber(7,false), (2,4) -> BingoCardNumber(20,false), (0,1) -> BingoCardNumber(8,true), (3,3) -> BingoCardNumber(18,false), (2,3) -> BingoCardNumber(3,false), (1,2) -> BingoCardNumber(9,false), (2,1) -> BingoCardNumber(23,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(13,false))

      // When
      val actual = Day4.checkForBingo(card).get

      // Then
      actual shouldBe card
    }

    "return none if it doesn't contain a fully marked row or column" in {
      // Given
      val card = Map((0,2) -> BingoCardNumber(21,false), (0,0) -> BingoCardNumber(22,false), (4,0) -> BingoCardNumber(0,false), (3,4) -> BingoCardNumber(15,false), (3,1) -> BingoCardNumber(4,false), (4,1) -> BingoCardNumber(24,false), (2,0) -> BingoCardNumber(17,false), (0,3) -> BingoCardNumber(6,false), (4,4) -> BingoCardNumber(19,false), (3,0) -> BingoCardNumber(11,false), (1,1) -> BingoCardNumber(2,false), (1,4) -> BingoCardNumber(12,false), (0,4) -> BingoCardNumber(1,false), (3,2) -> BingoCardNumber(16,false), (1,3) -> BingoCardNumber(10,false), (2,2) -> BingoCardNumber(14,false), (4,2) -> BingoCardNumber(7,false), (2,4) -> BingoCardNumber(20,false), (0,1) -> BingoCardNumber(8,false), (3,3) -> BingoCardNumber(18,false), (2,3) -> BingoCardNumber(3,false), (1,2) -> BingoCardNumber(9,false), (2,1) -> BingoCardNumber(23,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(13,false))

      // When
      val actual = Day4.checkForBingo(card)

      // Then
      actual shouldBe None
    }
  }

  "findWinningCard" should {
    "find the correct winning card" in {
      // Given
      val bingoCards = List(
        Map((0,2) -> BingoCardNumber(21,false), (0,0) -> BingoCardNumber(22,false), (4,0) -> BingoCardNumber(0,false), (3,4) -> BingoCardNumber(15,false), (3,1) -> BingoCardNumber(4,false), (4,1) -> BingoCardNumber(24,false), (2,0) -> BingoCardNumber(17,false), (0,3) -> BingoCardNumber(6,false), (4,4) -> BingoCardNumber(19,false), (3,0) -> BingoCardNumber(11,false), (1,1) -> BingoCardNumber(2,false), (1,4) -> BingoCardNumber(12,false), (0,4) -> BingoCardNumber(1,false), (3,2) -> BingoCardNumber(16,false), (1,3) -> BingoCardNumber(10,false), (2,2) -> BingoCardNumber(14,false), (4,2) -> BingoCardNumber(7,false), (2,4) -> BingoCardNumber(20,false), (0,1) -> BingoCardNumber(8,false), (3,3) -> BingoCardNumber(18,false), (2,3) -> BingoCardNumber(3,false), (1,2) -> BingoCardNumber(9,false), (2,1) -> BingoCardNumber(23,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(13,false)),
        Map((0,2) -> BingoCardNumber(19,false), (0,0) -> BingoCardNumber(3,false), (4,0) -> BingoCardNumber(22,false), (3,4) -> BingoCardNumber(12,false), (3,1) -> BingoCardNumber(17,false), (4,1) -> BingoCardNumber(5,false), (2,0) -> BingoCardNumber(0,false), (0,3) -> BingoCardNumber(20,false), (4,4) -> BingoCardNumber(6,false), (3,0) -> BingoCardNumber(2,false), (1,1) -> BingoCardNumber(18,false), (1,4) -> BingoCardNumber(21,false), (0,4) -> BingoCardNumber(14,false), (3,2) -> BingoCardNumber(25,false), (1,3) -> BingoCardNumber(11,false), (2,2) -> BingoCardNumber(7,false), (4,2) -> BingoCardNumber(23,false), (2,4) -> BingoCardNumber(16,false), (0,1) -> BingoCardNumber(9,false), (3,3) -> BingoCardNumber(24,false), (2,3) -> BingoCardNumber(10,false), (1,2) -> BingoCardNumber(8,false), (2,1) -> BingoCardNumber(13,false), (4,3) -> BingoCardNumber(4,false), (1,0) -> BingoCardNumber(15,false)),
        Map((0,2) -> BingoCardNumber(18,false), (0,0) -> BingoCardNumber(14,false), (4,0) -> BingoCardNumber(4,false), (3,4) -> BingoCardNumber(3,false), (3,1) -> BingoCardNumber(9,false), (4,1) -> BingoCardNumber(19,false), (2,0) -> BingoCardNumber(17,false), (0,3) -> BingoCardNumber(22,false), (4,4) -> BingoCardNumber(7,false), (3,0) -> BingoCardNumber(24,false), (1,1) -> BingoCardNumber(16,false), (1,4) -> BingoCardNumber(0,false), (0,4) -> BingoCardNumber(2,false), (3,2) -> BingoCardNumber(26,false), (1,3) -> BingoCardNumber(11,false), (2,2) -> BingoCardNumber(23,false), (4,2) -> BingoCardNumber(20,false), (2,4) -> BingoCardNumber(12,false), (0,1) -> BingoCardNumber(10,false), (3,3) -> BingoCardNumber(6,false), (2,3) -> BingoCardNumber(13,false), (1,2) -> BingoCardNumber(8,false), (2,1) -> BingoCardNumber(15,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(21,false))
      )

      val numbers = List(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)

      val expected = (Map((0,2) -> BingoCardNumber(18,false), (0,0) -> BingoCardNumber(14,true), (4,0) -> BingoCardNumber(4,true), (3,4) -> BingoCardNumber(3,false), (3,1) -> BingoCardNumber(9,true), (4,1) -> BingoCardNumber(19,false), (2,0) -> BingoCardNumber(17,true), (0,3) -> BingoCardNumber(22,false), (4,4) -> BingoCardNumber(7,true), (3,0) -> BingoCardNumber(24,true), (1,1) -> BingoCardNumber(16,false), (1,4) -> BingoCardNumber(0,true), (0,4) -> BingoCardNumber(2,true), (3,2) -> BingoCardNumber(26,false), (1,3) -> BingoCardNumber(11,true), (2,2) -> BingoCardNumber(23,true), (4,2) -> BingoCardNumber(20,false), (2,4) -> BingoCardNumber(12,false), (0,1) -> BingoCardNumber(10,false), (3,3) -> BingoCardNumber(6,false), (2,3) -> BingoCardNumber(13,false), (1,2) -> BingoCardNumber(8,false), (2,1) -> BingoCardNumber(15,false), (4,3) -> BingoCardNumber(5,true), (1,0) -> BingoCardNumber(21,true)),24)

      // When
      val actual = Day4.findWinningCard(bingoCards, numbers)

      // Then
      actual shouldBe expected
    }
  }

  "findLosingCard" should {
    "find the losing card" in {
      // Given
      val bingoCards = List(
        Map((0,2) -> BingoCardNumber(21,false), (0,0) -> BingoCardNumber(22,false), (4,0) -> BingoCardNumber(0,false), (3,4) -> BingoCardNumber(15,false), (3,1) -> BingoCardNumber(4,false), (4,1) -> BingoCardNumber(24,false), (2,0) -> BingoCardNumber(17,false), (0,3) -> BingoCardNumber(6,false), (4,4) -> BingoCardNumber(19,false), (3,0) -> BingoCardNumber(11,false), (1,1) -> BingoCardNumber(2,false), (1,4) -> BingoCardNumber(12,false), (0,4) -> BingoCardNumber(1,false), (3,2) -> BingoCardNumber(16,false), (1,3) -> BingoCardNumber(10,false), (2,2) -> BingoCardNumber(14,false), (4,2) -> BingoCardNumber(7,false), (2,4) -> BingoCardNumber(20,false), (0,1) -> BingoCardNumber(8,false), (3,3) -> BingoCardNumber(18,false), (2,3) -> BingoCardNumber(3,false), (1,2) -> BingoCardNumber(9,false), (2,1) -> BingoCardNumber(23,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(13,false)),
        Map((0,2) -> BingoCardNumber(19,false), (0,0) -> BingoCardNumber(3,false), (4,0) -> BingoCardNumber(22,false), (3,4) -> BingoCardNumber(12,false), (3,1) -> BingoCardNumber(17,false), (4,1) -> BingoCardNumber(5,false), (2,0) -> BingoCardNumber(0,false), (0,3) -> BingoCardNumber(20,false), (4,4) -> BingoCardNumber(6,false), (3,0) -> BingoCardNumber(2,false), (1,1) -> BingoCardNumber(18,false), (1,4) -> BingoCardNumber(21,false), (0,4) -> BingoCardNumber(14,false), (3,2) -> BingoCardNumber(25,false), (1,3) -> BingoCardNumber(11,false), (2,2) -> BingoCardNumber(7,false), (4,2) -> BingoCardNumber(23,false), (2,4) -> BingoCardNumber(16,false), (0,1) -> BingoCardNumber(9,false), (3,3) -> BingoCardNumber(24,false), (2,3) -> BingoCardNumber(10,false), (1,2) -> BingoCardNumber(8,false), (2,1) -> BingoCardNumber(13,false), (4,3) -> BingoCardNumber(4,false), (1,0) -> BingoCardNumber(15,false)),
        Map((0,2) -> BingoCardNumber(18,false), (0,0) -> BingoCardNumber(14,false), (4,0) -> BingoCardNumber(4,false), (3,4) -> BingoCardNumber(3,false), (3,1) -> BingoCardNumber(9,false), (4,1) -> BingoCardNumber(19,false), (2,0) -> BingoCardNumber(17,false), (0,3) -> BingoCardNumber(22,false), (4,4) -> BingoCardNumber(7,false), (3,0) -> BingoCardNumber(24,false), (1,1) -> BingoCardNumber(16,false), (1,4) -> BingoCardNumber(0,false), (0,4) -> BingoCardNumber(2,false), (3,2) -> BingoCardNumber(26,false), (1,3) -> BingoCardNumber(11,false), (2,2) -> BingoCardNumber(23,false), (4,2) -> BingoCardNumber(20,false), (2,4) -> BingoCardNumber(12,false), (0,1) -> BingoCardNumber(10,false), (3,3) -> BingoCardNumber(6,false), (2,3) -> BingoCardNumber(13,false), (1,2) -> BingoCardNumber(8,false), (2,1) -> BingoCardNumber(15,false), (4,3) -> BingoCardNumber(5,false), (1,0) -> BingoCardNumber(21,false))
      )

      val numbers = List(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)

      val expected = (Map((0,2) -> BingoCardNumber(19,false), (0,0) -> BingoCardNumber(3,false), (4,0) -> BingoCardNumber(22,false), (3,4) -> BingoCardNumber(12,false), (3,1) -> BingoCardNumber(17,true), (4,1) -> BingoCardNumber(5,true), (2,0) -> BingoCardNumber(0,true), (0,3) -> BingoCardNumber(20,false), (4,4) -> BingoCardNumber(6,false), (3,0) -> BingoCardNumber(2,true), (1,1) -> BingoCardNumber(18,false), (1,4) -> BingoCardNumber(21,true), (0,4) -> BingoCardNumber(14,true), (3,2) -> BingoCardNumber(25,false), (1,3) -> BingoCardNumber(11,true), (2,2) -> BingoCardNumber(7,true), (4,2) -> BingoCardNumber(23,true), (2,4) -> BingoCardNumber(16,true), (0,1) -> BingoCardNumber(9,true), (3,3) -> BingoCardNumber(24,true), (2,3) -> BingoCardNumber(10,true), (1,2) -> BingoCardNumber(8,false), (2,1) -> BingoCardNumber(13,true), (4,3) -> BingoCardNumber(4,true), (1,0) -> BingoCardNumber(15,false)),13)

      // When
      val actual = Day4.findLosingCard(bingoCards, numbers)

      // Then
      actual shouldBe expected
    }
  }

}
