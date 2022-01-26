import Helpers._

object Day13 extends App {
  val sample = """
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
"""

  case class Point(x: Int, y: Int)

  trait Line
  case class LineX(x: Int) extends Line
  case class LineY(y: Int) extends Line

  case class Input(points: Set[Point], instructions: List[Line])

  def formatted(source: String): Input = {
    val pointString :: foldString :: Nil = "(?m)^$".r.split(source.trim).toList

    val points = pointString.trim.linesIterator.map(l => {
      val xy = l.trim.split(",")
      Point(xy(0).toInt, xy(1).toInt)
    }).toSet

    val folds = foldString.trim.linesIterator.map(l => {
      val lp = l.stripPrefix("fold along ").split("=")
      if (lp(0) == "x") LineX(lp(1).toInt)
      else LineY(lp(1).toInt)
    }).toList

    Input(points, folds)
  }

  def range(a: Int, b: Int): List[Int] =
    if (a <= b) (a to b).toList
    else (b to a).toList.reverse 

  def foldTransition(line: Int): Map[Int, Int] =
    (range(line + 1, line * 2) zip range(line - 1, 0)).toMap

  def horizontalFold(points: Set[Point], y: Int): Set[Point] = {
    val transition = foldTransition(y)
    
    points.map(p => {
      transition.get(p.y) match {
        case Some(y) => Point(p.x, y)
        case None => p
      }
    })
  }

  def verticalFold(points: Set[Point], x: Int): Set[Point] = {
    val transition = foldTransition(x)
    
    points.map(p => {
      transition.get(p.x) match {
        case Some(x) => Point(x, p.y)
        case None => p
      }
    })
  }

  def fold(points: Set[Point], line: Line): Set[Point] =
    line match {
      case LineX(x) => verticalFold(points, x)
      case LineY(y) => horizontalFold(points, y)
    }

  def part1(input: Input): Unit = {
    val instructions = input.instructions
    val first = instructions.head
    val points = input.points
    val result = fold(points, first).size

    printGreen(s"$result")
  }

  def show(points: Set[Point]): String = {
    val height = points.map(_.y).max
    val width = points.map(_.x).max

    val lists = (0 to height).toList.map(y => {
      (0 to width).toList.map(x => {
        if (points.contains(Point(x, y))) "#" else "."
      })
    })
  
    lists.map(_.mkString).mkString("\n")
  }

  def part2(input: Input): Unit = {
    val instructions = input.instructions
    val points = input.points
    val result = instructions.foldLeft(points)((acc, instruction) => fold(acc, instruction))

    printGreen(s"${show(result)}")
  }

  val source = read("day13")

  part1(formatted(sample))
  println("---")
  part1(formatted(source))

  println("---------------")

  part2(formatted(sample))
  println("---")
  part2(formatted(source))
}
