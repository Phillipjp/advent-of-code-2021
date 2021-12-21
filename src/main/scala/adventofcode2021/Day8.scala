package adventofcode2021

object Day8 extends App{

  val input = Utils.readFileAsListOfString("day-8.txt")

  val part1Input = input.flatMap(line => line.split(" \\| ").last.split(" "))

  val result1 = part1Input.count(s => Seq(2,4,3,7).contains(s.length))

  println(result1)

  val part2Input = input.map{line =>
    val split = line.split(" \\| ")
    (
      split.head.split(" ").toList,
      split.last.split(" ").toList
    )
  }

  val result2 = part2Input.map{case (signalPatterns, output) =>
    val one = signalPatterns.find(_.length == 2).get.sorted
    val four = signalPatterns.find(_.length == 4).get.sorted
    val seven = signalPatterns.find(_.length == 3).get.sorted
    val eight = signalPatterns.find(_.length == 7).get.sorted

    val L = four.filter(c => !one.contains(c))

    val (two, three, five) = findTwoThreeFive(one, L, signalPatterns)
    val (zero, six, nine) = findZeroSixNine(four, L, signalPatterns)

    val mapping = Seq(zero, one, two, three, four, five, six, seven, eight, nine).zipWithIndex.toMap

    output.map(k => mapping(k.sorted)).mkString.toInt

  }.sum

  println(result2)


  def findTwoThreeFive(one: String, L: String, signalPatterns: Seq[String]): (String, String, String) = {
    val potentials = signalPatterns.filter(_.length == 5)
    val three = potentials.find(p => one.forall(c => p.contains(c))).get
    val five = potentials.find(p => L.forall(c => p.contains(c))).get
    val two = potentials.find(s => s != three && s != five).get
    (two.sorted, three.sorted, five.sorted)
  }

  def findZeroSixNine(four: String, L: String, signalPatterns: Seq[String]): (String, String, String) = {
    val potentials = signalPatterns.filter(_.length == 6)
    val nine = potentials.find(p => four.forall(c => p.contains(c))).get
    val six = potentials.filter(_ != nine).find(p => L.forall(c => p.contains(c))).get
    val zero = potentials.find(s => s != six && s != nine).get
    (zero.sorted, six.sorted, nine.sorted)
  }



}
