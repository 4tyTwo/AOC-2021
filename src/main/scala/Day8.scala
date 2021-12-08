import scala.io.Source

object Day8 {
  val filename = "priv/day8.txt"
  val (mixedInput, mixedOutput) = Source.fromFile(filename).getLines().toList.map(_.split(""" \| """).toList).map(l => (l(0), l(1))).unzip
  val inputs = mixedInput.map(_.split(" ").toList.map(_.toCharArray().toSet))
  val outputs = mixedOutput.map(_.split(" ").toList.map(_.toCharArray().toSet))

  val zero  = Set('a', 'b', 'c', 'e', 'f', 'g')
  val one   = Set('c', 'f')
  val two   = Set('a', 'c', 'd', 'e', 'g')
  val three = Set('a', 'c', 'd', 'f', 'g')
  val four  = Set('b', 'c', 'd', 'f')
  val five  = Set('a', 'b', 'd', 'f', 'g')
  val six   = Set('a', 'b', 'd', 'e', 'f', 'g')
  val seven = Set('a', 'c' , 'f')
  val eight = Set('a', 'b', 'c', 'd', 'e', 'f', 'g')
  val nine  = Set('a', 'b', 'c', 'd', 'f', 'g')

  val allDigits = List(zero, one, two, three, four, five, six, seven, eight, nine)

  val display = Map[Set[Char], Char](
    (zero -> '0'),
    (one -> '1'),
    (two -> '2'),
    (three -> '3'),
    (four -> '4'),
    (five -> '5'),
    (six -> '6'),
    (seven -> '7'),
    (eight -> '8'),
    (nine -> '9')
  )

  def solve1(): Int =
    (inputs, outputs).zipped.foldLeft(0)((acc, tpl) => acc + solveLine1(tpl._1, tpl._2))

  def solve2(): Int =
    (inputs, outputs).zipped.foldLeft(0)((acc, tpl) => acc + decodeOutput(mapSegments(tpl._1), tpl._2))

  def solveLine1(input: List[Set[Char]], output: List[Set[Char]]) = {
    val mapping = uniqueSegmentNumberDigits(input)
    output.foldLeft(0)((acc, v) => if(mapping(v)) acc + 1 else acc)
  }

  def signalsByOccurence(signals: List[Set[Char]]) =
    inverseMap(countOccurrences(signals))

  def uniqueSegmentNumberDigits(input: List[Set[Char]]): Set[Set[Char]] =
    input.foldLeft(Set[Set[Char]]())(
      (acc, s) =>
        s.size match {
          case 2 => acc + s
          case 3 => acc + s
          case 7 => acc + s
          case 4 => acc + s
          case _ => acc
        }
    )

  private def mapSegments(input: List[Set[Char]]): Map[Char, Char] = {
    val digits = uniqueSegmentNumberDigits(input)
    val notGuessedMixedNumbers = input.filter(s => !digits(s))
    val notGuessedCorrectNumbers = List(zero, two, three, five, six, nine)
    val invCorrect = signalsByOccurence(notGuessedCorrectNumbers)
    val inv = signalsByOccurence(notGuessedMixedNumbers)
    val mixed17 = signalsByOccurence(input.filter(s => s.size == 2 || s.size == 3))
    val correct17 = signalsByOccurence(List(one, seven))
    var finalMapping = Map(
      (inv.get(3).get.toList(0), invCorrect.get(3).get.toList(0)),
      (mixed17.get(1).get.toList(0), correct17.get(1).get.toList(0))
    )
    val mixed148 = signalsByOccurence(
        input.filter(s => s.size == 2 || s.size == 4 || s.size == 7).map(s => s.removedAll(finalMapping.keySet))
    )
    val digits148 = List(one, four, eight)
    val correct148 = signalsByOccurence(digits148.map(s => s.removedAll(finalMapping.values)))
    finalMapping = finalMapping + (mixed148.get(1).get.toList(0) -> correct148.get(1).get.toList(0))
    val mixedAllUnknown = signalsByOccurence(input.map(s => s.removedAll(finalMapping.keySet)))
    val correctAllUnknown = signalsByOccurence(allDigits.map(s => s.removedAll(finalMapping.values)))
    for ((k, v) <- mixedAllUnknown) {
      finalMapping = finalMapping + (v.toList(0) -> correctAllUnknown.get(k).get.toList(0))
    }
    finalMapping
  }

  private def countOccurrences(signals: List[Set[Char]]): Map[Char, Int] =
    signals.foldLeft(Map[Char, Int]())(
      (map: Map[Char, Int], set: Set[Char]) => {
        set.foldLeft(map)(
          (m, c) => m get c match {
            case None => m + (c -> 1)
            case Some(value) => m + (c -> (value + 1))
          }
        )
      }
    )

  private def inverseMap(map: Map[Char, Int]): Map[Int, Set[Char]] =
    map.foldLeft(Map[Int, Set[Char]]())(
      (m1: Map[Int, Set[Char]], m2: (Char, Int)) => {
        m1 get m2._2 match {
          case None => m1 + (m2._2 -> Set(m2._1))
          case Some(value) => m1 + (m2._2 -> (value + m2._1))
        }
      }
    )

  def decodeOutput(mapping: Map[Char, Char], outputLine: List[Set[Char]]): Int =
    Integer.parseInt(outputLine.map(s => s.map(mapping.get(_).get)).map(toDigit).mkString)

  def toDigit(s: Set[Char]): Char =
    display.get(s).get
}
