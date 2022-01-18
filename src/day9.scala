import Helpers._

object Day9 extends App {
  val sample = """
2199943210
3987894921
9856789892
8767896789
9899965678
"""

  case class Point(x: Int, y: Int)
  case class Height(value: Int, point: Point)
  type HeightMap = Map[Point, Height]

  def formatted(source: String): HeightMap = {
    val heightLists = source.trim.linesIterator.toList.map(_.toList.map(_.toString.toInt))

    heightLists.zipWithIndex.map {
      case (row, columnIndex) => row.zipWithIndex.map {
        case (height, rowIndex) => {
          val point = Point(rowIndex, columnIndex)
          (point -> Height(height, point))
        }
      }
    }.flatten.toMap
  }

  def adjacent(map: HeightMap, center: Point): Set[Height] = {
    val up    = map.get(Point(center.x,     center.y - 1))
    val down  = map.get(Point(center.x,     center.y + 1))
    val left  = map.get(Point(center.x - 1, center.y))
    val right = map.get(Point(center.x + 1, center.y))

    Set(up, down, left, right).filter(_.isDefined).map(_.get)
  }

  def part1(heightMap: HeightMap): Unit = {
    val lowHeightsMap = heightMap.filter {
      case (point, height) => height.value < adjacent(heightMap, point).map(_.value).min
    }

    val lowHeights = lowHeightsMap.values
    val result = lowHeights.map(_.value + 1).sum
    
    printGreen(s"$result")
  }

  def basin(map: HeightMap, center: Height): Set[Height] = {
    val adj =
      adjacent(map, center.point)
        .filter(h => h.value > center.value && h.value != 9)

    adj.map(h => basin(map, h)).flatten + center
  }

  def part2(heightMap: HeightMap): Unit = {
    val lowHeightsMap = heightMap.filter {
      case (point, height) => height.value < adjacent(heightMap, point).map(_.value).min
    }

    val basins = lowHeightsMap.values.map(h => basin(heightMap, h)).toList
    val result = basins.map(_.size).sorted.takeRight(3).product

    printGreen(s"$result")


  }

  val source = read("day9")

  part1(formatted(sample))
  println("---")
  part1(formatted(source))

  println("---------------")

  part2(formatted(sample))
  println("---")
  part2(formatted(source))
}
