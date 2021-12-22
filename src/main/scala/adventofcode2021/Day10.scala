package adventofcode2021

import scala.annotation.tailrec

object Day10 extends App{

  val subsystem = Utils.readFileAsListOfString("day-10.txt")
  val corruptedCharacters = getCorruptedCharacters(subsystem)
  val characterPoints = Map(
    ')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137
  )
  val answer = corruptedCharacters.map(c => characterPoints(c)).sum
  println(answer)

  val incompleteLines = getIncompleteLines(subsystem)
  val completedLines = completeLines(incompleteLines)
  val scores = scoreMissingChunks(completedLines).sorted
  val answer2 = scores.drop(scores.length/2).head
  println(answer2)



  def getCorruptedCharacters(subsystem: List[String]): List[Char] = {

    val openCharacterReciprocals = Map(
      '(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>'
    )
    def handleClosingCharacter(closingCharacter: Char, openCharacters: List[Char], line: String): Option[Char] = {

      if(openCharacterReciprocals(openCharacters.last) == closingCharacter)
        findCorruptedCharacter(line.tail, openCharacters.dropRight(1))
      else
        Some(closingCharacter)
    }

    @tailrec
    def findCorruptedCharacter(line: String, openCharacters: List[Char]): Option[Char] =
      line match {
        case "" => None
        case _ =>
          val char = line.head
          if (char == '(' || char == '[' || char == '{' || char == '<')
            findCorruptedCharacter(line.tail, openCharacters :+ char)
          else
            handleClosingCharacter(char, openCharacters, line)
      }

    subsystem.foldLeft(List[Char]()){ case (corruptedCharacters, line) =>
      findCorruptedCharacter(line, List()) match {
        case Some(c) => corruptedCharacters :+ c
        case None => corruptedCharacters
      }
    }
  }

  def getCorruptedLines(subsystem: List[String]): List[String] = {
    val openCharacterReciprocals = Map(
      '(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>'
    )
    def handleClosingCharacter(closingCharacter: Char, openCharacters: List[Char], line: String, originalLine: String): Option[String] = {
      if(openCharacterReciprocals(openCharacters.last) == closingCharacter)
        findCorruptedLine(line.tail, openCharacters.dropRight(1), originalLine)
      else
        Some(originalLine)
    }

    @tailrec
    def findCorruptedLine(line: String, openCharacters: List[Char], originalLine: String): Option[String] =
      line match {
        case "" => None
        case _ =>
          val char = line.head
          if (char == '(' || char == '[' || char == '{' || char == '<')
            findCorruptedLine(line.tail, openCharacters :+ char, originalLine)
          else
            handleClosingCharacter(char, openCharacters, line, originalLine)
      }

    subsystem.foldLeft(List[String]()){ case (corruptedLines, line) =>
      findCorruptedLine(line, List(), line) match {
        case Some(corrupted) => corruptedLines :+ corrupted
        case None => corruptedLines
      }
    }
  }

  def getIncompleteLines(subsystem: List[String]): List[String] = {
    val corruptedLines = getCorruptedLines(subsystem)
    subsystem.filter(line => !corruptedLines.contains(line))
  }

  def completeLines(lines: List[String]): List[String] = {
    val openCharacterReciprocals = Map(
      '(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>'
    )

    @tailrec
    def findMissingCharacters(line: String, openCharacters: List[Char]): String =
      line match {
        case "" => openCharacters.reverse.map(c => openCharacterReciprocals(c)).mkString
        case _ =>
          val char = line.head
          if (char == '(' || char == '[' || char == '{' || char == '<')
            findMissingCharacters(line.tail, openCharacters :+ char)
          else
            findMissingCharacters(line.tail, openCharacters.dropRight(1))
      }
    lines.map(line => findMissingCharacters(line, List()))
  }

  def scoreMissingChunks(chunks: List[String]): List[Long] = {
    val characterScores = Map(
      ')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4
    )
    chunks.map{chunk =>
      chunk.foldLeft(0L){case(score, char) =>
        score * 5 + characterScores(char)
      }
    }
  }

}
