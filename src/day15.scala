import Helpers._
import scala.math.abs


object Day15 extends App {
  val sample = """
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
"""

  case class Point(x: Int, y: Int)
  case class Field(cave: Map[Point, Int], width: Int, height: Int, start: Point, finish: Point)
  case class Path(points: List[Point], total: Int)

  def forked(base: Point, field: Field, approximately: Boolean): List[Point] = {
    val rightPoint = Point(base.x + 1, base.y)
    val bottomPoint = Point(base.x, base.y + 1)
    val leftPoint = Point(base.x - 1, base.y)
    val upPoint = Point(base.x, base.y - 1)

    val list =
      if (approximately) List(rightPoint, bottomPoint)
      else List(rightPoint, bottomPoint, leftPoint, upPoint)

    list.filter(field.cave.contains(_))
  }

  def forked(base: Path, field: Field, approximately: Boolean): List[Path] = {
    val points = forked(base.points.last, field, approximately)
    points.map(p => Path(base.points :+ p, base.total + field.cave(p)))
  }

  def pruned(pathes: List[Path]): List[Path] = {
    pathes.foldLeft(Map.empty[Point, Path])((acc, path) => {
      val last = path.points.last
      if (acc.contains(last)) {
        if (acc(last).total < path.total) acc
        else acc.updated(last, path)
      } else {
        acc.updated(last, path)
      }
    }).view.values.toList
  }

  def hasCycle(path: Path): Boolean =
    path.points.toSet.size != path.points.size

  def step(pathes: List[Path], field: Field, totalLimit: Option[Int], i: Int = 1): List[Path] = {
    val refined = pruned(pathes).filterNot(hasCycle(_))
    val filtered = totalLimit match {
      case Some(limit) => {
        if (refined.size > 1000) {
          val progress: Path => Int = path => path.points.last.x + path.points.last.y
          val trashold = refined.map(progress).sorted.apply((refined.size * 0.05).toInt)
          
          refined
            .filter(_.total <= limit)
            .filter(progress(_) >= trashold)
        } else refined
      }
      case None => refined
    }

    val approximately = totalLimit.fold(true)(_ => false)
    val next = filtered.flatMap(path => {
      if (path.points.last != field.finish) forked(path, field, approximately)
      else List(path)
    })

    if (next.size == 1) next
    else step(next, field, totalLimit, i + 1)
  }

  def formatted(source: String): Field = {
    def formattedLine(line: String) = line.toList.map(char => s"$char").map(_.toInt)
    val lists = source.trim.linesIterator.toList.map(formattedLine(_))

    val pointRisk = for {
      (row, y) <- lists.zipWithIndex
      (risk, x) <- row.zipWithIndex
    } yield Point(x, y) -> risk

    val maxX = pointRisk.map(_._1.x).max
    val maxY = pointRisk.map(_._1.y).max

    Field(pointRisk.toMap, maxX + 1, maxY + 1, Point(0, 0), Point(maxX, maxY))
  }

  def part1(field: Field): Unit = {
    val approximately = step(List(Path(List(field.start), 0)), field, None).head
    val result = step(List(Path(List(field.start), 0)), field, Some(approximately.total)).head
   
    printGreen(s"${result.total}")
  }

  def largeField(field: Field): Field = {
    def incr(risk: Int) = if (risk == 9) 1 else risk + 1
    def rightShift(cave: Map[Point, Int]): Map[Point, Int] =
      cave.map(pr => {
        val newPoint = Point(pr._1.x + field.width, pr._1.y)
        newPoint -> incr(pr._2)
      })

    def downShift(cave: Map[Point, Int]): Map[Point, Int] =
      cave.map(pr => {
        val newPoint = Point(pr._1.x, pr._1.y + field.height)
        newPoint -> incr(pr._2)
      })

    val f2 = rightShift(field.cave)
    val f3 = rightShift(f2)
    val f4 = rightShift(f3)
    val f5 = rightShift(f4)
    
    val c1 = field.cave ++ f2 ++ f3 ++ f4 ++ f5

    val c2 = downShift(c1)
    val c3 = downShift(c2)
    val c4 = downShift(c3)
    val c5 = downShift(c4)

    val finalCave = c1 ++ c2 ++ c3 ++ c4 ++ c5

    val maxX = finalCave.map(_._1.x).max
    val maxY = finalCave.map(_._1.y).max

    Field(finalCave, maxX + 1, maxY + 1, Point(0, 0), Point(maxX, maxY))
  }

  def part2(field: Field): Unit = {
    val large = largeField(field)
    val approximately = step(List(Path(List(field.start), 0)), large, None).head
    val result = step(List(Path(List(field.start), 0)), large, Some(approximately.total)).head

    printGreen(s"${result.total}")
  }

  val source = read("day15")

  part1(formatted(sample))
  println("---")
  part1(formatted(source))

  println("---------------")

  part2(formatted(sample))
  println("---")
  part2(formatted(source))
}
