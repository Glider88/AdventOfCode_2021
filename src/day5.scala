import Helpers._

object Day5 extends App {
  val sample = """
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"""

  case class Point(x: Int, y: Int)
  case class Line(first: Point, second: Point)

  def range(a: Int, b: Int): List[Int] =
    if (a <= b) (a to b).toList
    else (b to a).toList.reverse 

  def horizontal(line: Line): List[Point] =
    if (line.first.y == line.second.y) {
      range(line.first.x, line.second.x).map(Point(_, line.first.y))
    } else {
      List.empty[Point]
    }

  def vertical(line: Line): List[Point] =
    if (line.first.x == line.second.x) {
      range(line.first.y, line.second.y).map(Point(line.first.x, _))
    } else {
      List.empty[Point]
    }
  
  def diagonal(line: Line): List[Point] = {
    if (line.first.x != line.second.x && line.first.y != line.second.y) {
      val xs = range(line.first.x, line.second.x)
      val ys = range(line.first.y, line.second.y)
      (xs zip ys).map(xy => Point(xy._1, xy._2))
    } else {
      List.empty[Point]
    }
  }

  def formatted(source: String): List[Line] = {
    source.trim.linesIterator.map(lineString => {
      val points = lineString.trim.split("->").map(pointString => {
        val pair = pointString.trim.split(",").map(_.trim.toInt)
        Point(pair(0), pair(1))
      })
      Line(points(0), points(1))
    }).toList
  }

  def part1(lines: List[Line]): Unit = {
    val allPoints = lines.map(line => horizontal(line) ++ vertical(line)).flatten
    val doubles = allPoints.foldLeft((Set.empty[Point], Set.empty[Point]))((acc, p) => {
      if (acc._1.contains(p)) {
        (acc._1 + p, acc._2 + p)
      } else {
        (acc._1 + p, acc._2)
      }
    })
    printGreen(s"${doubles._2.size}")
  }
  
  def part2(lines: List[Line]): Unit = {
    val allPoints = lines.map(line => horizontal(line) ++ vertical(line) ++ diagonal(line)).flatten
    val doubles = allPoints.foldLeft((Set.empty[Point], Set.empty[Point]))((acc, p) => {
      if (acc._1.contains(p)) {
        (acc._1 + p, acc._2 + p)
      } else {
        (acc._1 + p, acc._2)
      }
    })
    printGreen(s"${doubles._2.size}")
  }

  val source = read("day5")
  val lines = formatted(source)
  val sampleLines = formatted(sample)

  part1(sampleLines)
  println("---")
  part1(lines)

  println("---------------")

  part2(sampleLines)
  println("---")
  part2(lines)
}
