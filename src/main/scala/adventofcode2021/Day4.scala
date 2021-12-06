package adventofcode2021

import scala.annotation.tailrec

object Day4 extends App {

  type BingoCard = Map[(Int, Int), BingoCardNumber]

  def parseBingoCards(input: List[String]): List[BingoCard] = {

    @tailrec
    def makeCards(input: List[String], cards: List[BingoCard]): List[BingoCard] = {
      input match {
        case Nil => cards
        case _ =>
          val rawCard = input.take(5).flatMap( l => l.trim.replaceAll("  ", " ")split(" ")).zipWithIndex
          val newCard = rawCard.map { case (number, index) =>
            val num = number.toInt
            val y = index / 5
            val x = index - (y * 5)
            (x, y) -> BingoCardNumber(num, marked = false)
          }
            .toMap
          makeCards(input.drop(6), cards :+ newCard)
      }
    }

    makeCards(input, List[BingoCard]())
  }

  def markCard(bingoCard: BingoCard, number: Int): BingoCard = {
    val updateCard = bingoCard.find { case (_, bingoCardNumber) => bingoCardNumber.value == number }
    updateCard match{
      case None => bingoCard
      case Some(((x,y), card)) => bingoCard + ((x,y) -> card.mark())
    }
  }

  def checkForBingo(bingoCard: BingoCard): Option[BingoCard] = {
    val markedCards: Map[(Int, Int), BingoCardNumber] = bingoCard.filter{case (_, cardNumber) => cardNumber.marked}

    val isRowLine = checkForLine(markedCards, (_, y) => y)
    val isColumnLine = checkForLine(markedCards, (x,_) => x)

    if(isRowLine || isColumnLine)
      Some(bingoCard)
    else
      None
  }

  private def checkForLine(markedCards: BingoCard, line: (Int, Int) => Int): Boolean = {
    markedCards
      .groupBy{case (coord, _) => line(coord._1, coord._2)}
      .map{case (_, card) =>
        card.values.toList
      }
      .exists(numbers => numbers.size == 5)
  }

  def findWinningCard(bingoCards: List[BingoCard], numbers: List[Int]): (BingoCard, Int) = {


    def playTurn(bingoCards: List[BingoCard], numbers: List[Int]): (BingoCard, Int) = {
      val markedCards = bingoCards.map(bingoCard => markCard(bingoCard, numbers.head))
      val winningCard = markedCards.map(bingoCard => checkForBingo(bingoCard)).find(_.nonEmpty).flatten
      winningCard match {
        case None => playTurn(markedCards, numbers.tail)
        case Some(card) => (card, numbers.head)
      }
    }

    playTurn(bingoCards, numbers)
  }

  def findLosingCard(bingoCards: List[BingoCard], numbers: List[Int]): (BingoCard, Int) = {

    @tailrec
    def playTurn(bingoCards: List[BingoCard], numbers: List[Int]): (BingoCard, Int) = {
      val markedCards = bingoCards.map(bingoCard => markCard(bingoCard, numbers.head))
      val winningCards = markedCards.filter(bingoCard => checkForBingo(bingoCard).nonEmpty)
      val remainingCards = markedCards.filter(bingoCard => !winningCards.contains(bingoCard))
      remainingCards match {
        case Nil if winningCards.size == 1 => (winningCards.head, numbers.head)
        case Nil if winningCards.size != 1 => throw new RuntimeException("There needs to be exactly one winning card if there are no remaining cards")
        case _ => playTurn(remainingCards, numbers.tail)
      }
    }

    playTurn(bingoCards, numbers)
  }

  def calculateBoardScore(board: BingoCard, finalNumber: Int): Int = {
    board.values.filter(!_.marked).map(_.value).sum * finalNumber
  }

  val input = Utils.readFileAsListOfString("day-4-part-1.txt")
  val numbers = input.head.split(",").map(_.toInt).toList
  val rawCards = input.drop(2)

  val bingoCards = parseBingoCards(rawCards)

  val (winningBoard, firstWinningNumber) = findWinningCard(bingoCards, numbers)
  val winningBoardScore = calculateBoardScore(winningBoard, firstWinningNumber)
  println(s"Winning Board Score: $winningBoardScore")

  val (losingBoard, finalWinningNumber) = findLosingCard(bingoCards, numbers)
  val losingBoardScore = calculateBoardScore(losingBoard, finalWinningNumber)
  println(s"Losing Board Score: $losingBoardScore")

}

case class BingoCardNumber(value: Int, marked: Boolean){
  def mark(): BingoCardNumber = this.copy(marked = true)
}
