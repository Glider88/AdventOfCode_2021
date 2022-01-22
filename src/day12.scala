import Helpers._

object Day12 extends App {
  val sample1 = """
start-A
start-b
A-c
A-b
b-d
A-end
b-end
"""

  val sample2 = """
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
"""

  val sample3 = """
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
"""

  type Cave = String
  type AdjacencyList = Map[Cave, Set[Cave]]

  def formatted(source: String): AdjacencyList = {
    val paths = source.trim.linesIterator.map(line => {
      val caves = line.trim.split("-").map(_.trim)
      (caves(0), caves(1))
    }).toList

    val caves = paths.map(p => List(p._1, p._2)).flatten.distinct

    val cavesMap = caves.map(cave => {
      val values = paths
        .filter(p => if (p._1 == cave || p._2 == cave) true else false)
        .map {
          case (`cave`, t) => t
          case (t, `cave`) => t
          case _ => throw new RuntimeException(s"'$cave' isolated")
        }

      cave -> values.toSet.filter(_ != "start")
    }).toMap

    cavesMap ++ Map("end" -> Set.empty[Cave])
  }

  def isBig(cave: Cave): Boolean = cave.capitalize == cave
  def isSmall(cave: Cave): Boolean = !isBig(cave)
  def smallCaves(adjacencyList: AdjacencyList): Set[Cave] = adjacencyList.keySet.filter(isSmall(_))

  trait Strategy {
    def available(visited: List[Cave], verifiable: Set[Cave]): Set[Cave]
  }

  case class SingleStrategy() extends Strategy {
    def available(visited: List[Cave], verifiable: Set[Cave]): Set[Cave] = {
      val (bigCaves, smallCaves) = verifiable.partition(isBig(_))
      val availableSmallCaves = smallCaves.toList diff visited
      bigCaves ++ availableSmallCaves
    }
  }

  case class DoubleStrategy() extends Strategy {
    def hasDoubles(caves: List[Cave]): Boolean = 
      if (caves.distinct.size == caves.size) false
      else if (caves.distinct.size + 1 == caves.size) true
      else throw new RuntimeException(s"Multiple doubles $caves") 

    def available(visited: List[Cave], verifiable: Set[Cave]): Set[Cave] =
      if (hasDoubles(visited)) {
        SingleStrategy().available(visited, verifiable)
      } else {
        verifiable
      }
  }

  case class Filter(private val strategy: Strategy, private val visited: List[Cave] = List.empty[Cave]) {
    def available(caves: Set[Cave]): Set[Cave] = strategy.available(visited, caves)
    def visit(cave: Cave): Filter =
      if (isSmall(cave)) this.copy(visited = this.visited :+ cave)
      else this
  }

  def paths(adjacencyList: AdjacencyList, from: Cave, prevFilter: Filter): Set[List[Cave]] = {
    val nextCaves = adjacencyList(from)
    val filter = prevFilter.visit(from)
    val availableCaves = filter.available(nextCaves)

    if (from == "end") {
      Set(List("end"))
    } else if (availableCaves.isEmpty) {
      Set.empty[List[Cave]]
    } else {
      availableCaves.flatMap(to => {
        paths(adjacencyList, to, filter).map(tail => from :: tail)
      })
    }
  }

  def part1(adjacencyList: AdjacencyList): Unit = {
    val result =
      paths(adjacencyList, "start", Filter(SingleStrategy()))
        .filter(_.last == "end")
        .size

    printGreen(s"$result")
  }

  def part2(adjacencyList: AdjacencyList): Unit = {
    val result =
      paths(adjacencyList, "start", Filter(DoubleStrategy()))
        .filter(_.last == "end")
        .size

    printGreen(s"$result")
  }

  val source = read("day12")

  part1(formatted(sample1)) //   10
  part1(formatted(sample2)) //   19
  part1(formatted(sample3)) //  226
  println("---")
  part1(formatted(source))  // 5212
  println("---------------")

  part2(formatted(sample1)) //     36
  part2(formatted(sample2)) //    103
  part2(formatted(sample3)) //   3509
  println("---")
  part2(formatted(source))  // 134862
}
