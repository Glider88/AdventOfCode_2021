import Helpers._

object Day14 extends App {
  val sample = """
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
"""

  case class Input(polymerTemplate: String, pairInsertion: Map[String, String])

  def formatted(source: String): Input = {
    val polymerTemplate :: pairInsertionString :: Nil = "(?m)^$".r.split(source.trim).toList
    val pairMap = 
      pairInsertionString.trim.linesIterator
        .map(_.trim.split(" -> "))
        .map(l => l(0) -> l(1))
        .toMap

    Input(polymerTemplate.trim, pairMap)
  }

  def score(polymerTemplate: String): Int = {
    val sizes = polymerTemplate.groupBy(a => a).map(_._2.size).toList.sorted
    sizes.last - sizes.head
  }

  def step(pairInsertion: Map[String, String], polymerTemplate: String): String = {
    val pairs = polymerTemplate.toList.sliding(2).map(_.mkString)
    val newPairs = pairs.map(pair => {
      pairInsertion.get(pair) match {
        case Some(element) => s"${pair(0)}$element${pair(1)}"
        case None => pair
      }
    })

    val first :: other = newPairs.toList
    (first :: other.map(_.tail)).mkString
  }

  def fastStep(headMap: Map[String, String], tailMap: Map[String, String], polymerTemplate: String): String = {
    val templateArray = polymerTemplate.toArray
    var result = new StringBuilder()
    
    var k1 = templateArray(0)
    var k2 = templateArray(1)
    var key = s"$k1$k2"

    if (headMap.contains(key)) {
      result.append(headMap(key))
    } else {
      result.append(key)
    }

    var i = 2
    val length = templateArray.length
    while (i < length) {
      k1 = templateArray(i - 1)
      k2 = templateArray(i)
      key = s"$k1$k2"
      if (tailMap.contains(key)) {
        result.append(tailMap(key))
      } else {
        result.append(k2)
      }
      i += 1
    }

    result.toString
  }

  def part1(input: Input): Unit = {
    val finalTemplate = (1 to 10).foldLeft(input.polymerTemplate)((acc, i) => {
      step(input.pairInsertion, acc)
    })

    val result = score(finalTemplate)

    printGreen(s"${result}")
  }

  def part2(input: Input): Unit = {
    val defaultMap =
      input.pairInsertion.map(pairInsert => {
        val key = pairInsert._1
        val left = key(0)
        val right = key(1)
        val middle = pairInsert._2
        key -> s"$left$middle$right"
      })

    def firstLeaf(node: String, step: Int): String =
      (1 to step).foldLeft(node)((acc, _) => defaultMap(acc).slice(0, 2))

    def multiply(countMap: Map[String, Long], multiplier: Long) =
      countMap.view.mapValues(_ * multiplier).toMap

    def sum(countMaps: List[Map[String, Long]]): Map[String, Long] = {
      val keys = countMaps.flatMap(_.keySet).toSet

      keys.map(k => {
        k -> countMaps.map(m => m.get(k).getOrElse(0L)).sum
      }).toMap
    }

    def toCountMap[A](list: List[A]): Map[A, Long] =
      list.groupBy(a => a).map(e => (e._1, e._2.length.toLong))

    def nSteps(node: String, n: Int): Map[String, Long] = {
      val doubles = node.sliding(2).toList
      val listNodes = (1 to n).foldLeft(doubles)((acc, _) =>
        acc.map(defaultMap(_)).flatMap(_.sliding(2))
      )

      toCountMap(listNodes)
    }

    val headNodes = input.polymerTemplate.sliding(2).toList
    val headCountMap = toCountMap(headNodes)

    val eventual = (1 to 4).foldLeft(headCountMap)((acc, _) => {
      sum(acc.view.map(nc => multiply(nSteps(nc._1, 10), nc._2)).toList)
    })

    val charCountList = eventual.toList.map(nc => (nc._1(1), nc._2))

    val charLong = charCountList.foldLeft(Map.empty[Char, Long])((acc, charCount) => {
      acc.get(charCount._1) match {
        case Some(count) => acc ++ Map(charCount._1 -> (charCount._2 + count))
        case None => acc ++ Map(charCount._1 -> charCount._2)
      }
    })

    val firstCharFinalLeafs = firstLeaf(input.polymerTemplate.slice(0, 2), 40)(0)
    val correctCharLong = charLong.updated(firstCharFinalLeafs, charLong(firstCharFinalLeafs) + 1)
    val sortedCharLong = correctCharLong.toList.sortWith(_._2 < _._2)
    val result = sortedCharLong.last._2 - sortedCharLong.head._2

    printGreen(s"${result}")
  }

  val source = read("day14")

  part1(formatted(sample))
  println("---")
  part1(formatted(source))

  println("---------------")

  part2(formatted(sample))
  println("---")
  part2(formatted(source))
}
