import Helpers._

object Day7 extends App {
  val sample = """
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
"""

  val digitToCode = Map(
    0 -> Set('a', 'b', 'c', 'e', 'f', 'g'),
    1 -> Set('c', 'f'),
    2 -> Set('a', 'c', 'd', 'e', 'g'),
    3 -> Set('a', 'c', 'd', 'f', 'g'),
    4 -> Set('b', 'c', 'd', 'f'),
    5 -> Set('a', 'b', 'd', 'f', 'g'),
    6 -> Set('a', 'b', 'd', 'e', 'f', 'g'),
    7 -> Set('a', 'c', 'f'),
    8 -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    9 -> Set('a', 'b', 'c', 'd', 'f', 'g')
  )
  
  val uniquePatterns = List(
    Set('c', 'f'),
    Set('a', 'c', 'f'),
    Set('b', 'c', 'd', 'f'),
    Set('a', 'b', 'c', 'd', 'e', 'f', 'g')
  )

  val codeToDigit = for ((d, c) <- digitToCode) yield (c, d)

  type Chars = Set[Char]
  case class Translate(unknown: Chars, known: Chars)

  case class InputLine(signals: Set[Chars], output: List[Chars])

  def formatted(source: String): List[InputLine] =
    source.trim.linesIterator.toList.map(line => {
      val left = line.split("\\|").apply(0).trim
      val right = line.split("\\|").apply(1).trim
      val sig = left.split(" ").toSet.map(_.toSet)
      val out = right.split(" ").toList.map(_.toSet)
      InputLine(sig, out)
    })

  def part1(lines: List[InputLine]): Unit = {
    val uniqSizes = uniquePatterns.map(_.size)
    def uniq(word: Chars): Int =
      if (uniqSizes.contains(word.size)) 1 else 0

    val outputWords = lines.map(_.output).flatten
    val result = outputWords.map(uniq(_)).sum
    printGreen(s"${result}")
  }
  
  def diff(t1: Translate, t2: Translate): Option[Translate] = {
    val unknown = t1.unknown diff t2.unknown
    val known = t1.known diff t2.known
    
    if (unknown.nonEmpty && known.nonEmpty) {
      Some(Translate(unknown, known))
    } else {
      None
    }
  }

  def show(ps: List[Translate]): String =
    ps.map(t => s"${t.unknown.mkString} ${t.known.mkString}").mkString("\n")

  def multiDiff(base: Translate, diffList: List[Translate]): Option[Translate] =
    diffList.foldLeft(Option(base))((acc, t2) => acc match {
      case Some(t1) => diff(t1, t2)
      case None => None 
    })


  def simplified(init: List[Translate]): List[Translate] = {
    val first :: tail = init
    val raw = tail.foldLeft(List(Option(first)))(
      (acc, t) =>
        acc :+ multiDiff(t, acc.flatten)
    ) 
  
    raw.filter(_.isDefined).flatten
  }

  def partialTranslation(dictionary: List[Translate], word: Chars): Chars =
    dictionary.map(t => {
      if (t.unknown.subsetOf(word)) {
        t.known
      } else {
        Set.empty[Char]
      }
    })
    .flatten
    .toSet

  def decodeLine(line: InputLine): Int = {
    val signalMap = line.signals.map(w => (w.size, w)).toMap
    val uniqMap = uniquePatterns.map(w => (w.size -> w)).toMap

    val initDictionary =
      uniquePatterns
        .map(_.size)
        .map(s => Translate(signalMap(s), uniqMap(s)))

    val dictionary = simplified(initDictionary)

    val translations = line.output.map(unknown => {
      val conditates = digitToCode.filter {
        case (_, code) => code.size == unknown.size
      }

      val know = conditates.filter {
        case (_, conditate) => partialTranslation(dictionary, unknown).subsetOf(conditate)
      }.values

      if (know.size != 1) {
        throw new RuntimeException(s"Can't translate '$unknown' with dictionary $dictionary")
      } else {
        know.head
      }
    })

    translations.map(codeToDigit(_)).mkString.toInt
  }

  def part2(lines: List[InputLine]): Unit = {
    printGreen(s"${lines.map(decodeLine(_)).sum}")
  }

  val source = read("day8")

  part1(formatted(sample))
  println("---")
  part1(formatted(source))

  println("---------------")

  part2(formatted(sample))
  println("---")
  part2(formatted(source))
}

/*

  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

0 - abc efg - 6
1 -   c  f  - 2 +
2 - a cde g - 5
3 - a cd fg - 5
4 -  bcd f  - 4 +
5 - ab d fg - 5
6 - ab defg - 6
7 - a c  f  - 3 +
8 - abcdefg - 7 +
9 - abcd fg - 6



acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf

ab cf
dab acf
eafb bcdf
acedgfb abcdefg


ab cf
d a
ef bd
cedgf abdeg

ab cf
d a
ef bd
cg eg

{cb} abd - {fc} acf - {cb} abd - {cf} acf

5 - 3 - 5 - 3
*/

