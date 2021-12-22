package adventofcode2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day10Spec extends AnyWordSpec with Matchers {

  "getCorruptedCharacters" should {
    "get a list of the corrupted characters" in {
      // Given
      val subsystem = List(
        "[({(<(())[]>[[{[]{<()<>>",
        "[(()[<>])]({[<{<<[]>>(",
        "{([(<{}[<>[]}>{[]{[(<()>",
        "(((({<>}<{<{<>}{[]{[]{}",
        "[[<[([]))<([[{}[[()]]]",
        "[{[{({}]{}}([{[{{{}}([]",
        "{<[[]]>}<{[{[{[]{()[[[]",
        "[<(<(<(<{}))><([]([]()",
        "<{([([[(<>()){}]>(<<{{",
        "<{([{{}}[<[[[<>{}]]]>[]]"
      )
      // When
      val actual = Day10.getCorruptedCharacters(subsystem)

      // Then
      actual should contain theSameElementsAs List('}', ')', ']', ')', '>')
    }
  }

  "getCorruptedLines" should {
    "get only the corrupted lines" in {
      // Given
      val subsystem = List(
        "[({(<(())[]>[[{[]{<()<>>",
        "[(()[<>])]({[<{<<[]>>(",
        "{([(<{}[<>[]}>{[]{[(<()>",
        "(((({<>}<{<{<>}{[]{[]{}",
        "[[<[([]))<([[{}[[()]]]",
        "[{[{({}]{}}([{[{{{}}([]",
        "{<[[]]>}<{[{[{[]{()[[[]",
        "[<(<(<(<{}))><([]([]()",
        "<{([([[(<>()){}]>(<<{{",
        "<{([{{}}[<[[[<>{}]]]>[]]"
      )
      val expected = List(
        "{([(<{}[<>[]}>{[]{[(<()>",
        "[[<[([]))<([[{}[[()]]]",
        "[{[{({}]{}}([{[{{{}}([]",
        "[<(<(<(<{}))><([]([]()",
        "<{([([[(<>()){}]>(<<{{"
      )
      // When
      val actual = Day10.getCorruptedLines(subsystem)

      // Then
      actual should contain theSameElementsAs expected
    }
  }
  "getIncompleteLines" should {
    "only get lines that are incomplete" in {
      // Given
      val subsystem = List(
        "[({(<(())[]>[[{[]{<()<>>",
        "[(()[<>])]({[<{<<[]>>(",
        "{([(<{}[<>[]}>{[]{[(<()>",
        "(((({<>}<{<{<>}{[]{[]{}",
        "[[<[([]))<([[{}[[()]]]",
        "[{[{({}]{}}([{[{{{}}([]",
        "{<[[]]>}<{[{[{[]{()[[[]",
        "[<(<(<(<{}))><([]([]()",
        "<{([([[(<>()){}]>(<<{{",
        "<{([{{}}[<[[[<>{}]]]>[]]"
      )
      val expected = List(
        "[({(<(())[]>[[{[]{<()<>>",
        "[(()[<>])]({[<{<<[]>>(",
        "(((({<>}<{<{<>}{[]{[]{}",
        "{<[[]]>}<{[{[{[]{()[[[]",
        "<{([{{}}[<[[[<>{}]]]>[]]"
      )
      // When
      val actual = Day10.getIncompleteLines(subsystem)

      // Then
      actual should contain theSameElementsAs expected
    }
  }

  "completeLines" should {
    "get the missing characters required to complete the lines" in {
      // Given
      val lines = List(
        "[({(<(())[]>[[{[]{<()<>>",
        "[(()[<>])]({[<{<<[]>>(",
        "(((({<>}<{<{<>}{[]{[]{}",
        "{<[[]]>}<{[{[{[]{()[[[]",
        "<{([{{}}[<[[[<>{}]]]>[]]"
      )

      val expected = List(
        "}}]])})]",
        ")}>]})",
        "}}>}>))))",
        "]]}}]}]}>",
        "])}>"
      )
      // When
      val actual = Day10.completeLines(lines)

      // Then
      actual should contain theSameElementsAs expected
    }
  }

  "scoreMissingChunks" should{
    "get the spores for the missing chunks" in {
      val chunks = List(
        "}}]])})]",
        ")}>]})",
        "}}>}>))))",
        "]]}}]}]}>",
        "])}>"
      )

      val expected = List(
        288957, 5566, 1480781, 995444, 294
      )
      // When
      val actual = Day10.scoreMissingChunks(chunks)

      // Then
      actual should contain theSameElementsAs expected
    }
  }
}
