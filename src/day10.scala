import Helpers._

object Day10 extends App {
  val sample = """
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
"""

  def formatted(source: String): List[String] = {
    source.trim.linesIterator.map(_.trim).toList
  }

  def delEmpty(str: String): String =
    str.replace("{}", "").replace("()", "").replace("<>", "").replace("[]", "")

  def simplified(str: String): String = {
    val newStr = delEmpty(str)
    if (newStr == str) {
      str
    } else {
      simplified(newStr)
    }
  }

  def part1(lines: List[String]): Unit = {
    val scores: Map[Char, Int] = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )

    val simplifiedLines = lines.map(simplified(_))
    val ends = scores.keySet
    val illegalChars = simplifiedLines.map(line => {
      line.filter(ends.contains(_))
    }).filter(_.nonEmpty).map(_.head)

    val result = illegalChars.map(scores(_)).sum

    printGreen(s"$result")
  }

  def part2(lines: List[String]): Unit = {
    val scores: Map[Char, Int] = Map(
      ')' -> 1,
      ']' -> 2,
      '}' -> 3,
      '>' -> 4
    )
    val ends = scores.keySet
    val pair: Map[Char, Char] = Map(
      ')' -> '(',
      ']' -> '[',
      '}' -> '{',
      '>' -> '<',
      '(' -> ')',
      '[' -> ']',
      '{' -> '}',
      '<' -> '>'
    )
    val simplifiedLines = lines.map(simplified(_))
    val incompletedLines = simplifiedLines.filter(line => (line.toSet intersect ends).isEmpty)
    val closingLines = incompletedLines.map(line => line.map(pair(_)).reverse)

    val scoreList = closingLines.map(line => {
      line.toList.foldLeft(0L)((acc, chr) => acc * 5 + scores(chr))
    }).sorted

    val size = scoreList.size
    if (size % 2 == 0) {
      throw new RuntimeException(s"Invalid data, even size: $size")
    }

    val middle = (size / 2).toInt
    val result = scoreList(middle)

    printGreen(s"$result")
  }

  val source = read("day10")

  part1(formatted(sample))
  println("---")
  part1(formatted(source))

  println("---------------")

  part2(formatted(sample))
  println("---")
  part2(formatted(source))
}
