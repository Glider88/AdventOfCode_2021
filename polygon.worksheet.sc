
val sample = """
2199943210
3987894921
9856789892
8767896789
9899965678
"""

/*
  0123456789

0 2199943210
1 3987894921
2 9856789892
3 8767896789
4 9899965678
*/

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

def basin(map: HeightMap, center: Height): Set[Height] = {
  val adj =
    adjacent(map, center.point)
      .filter(_.value == center.value + 1 && center.value + 1 != 9)

  adj.map(h => basin(map, h)).flatten + center
}

def show(h: Height): String = s"${h.value} - ${h.point.x} ${h.point.y}"

val heightMap = formatted(sample)

val lowHeightsMap = heightMap.filter {
  case (point, height) => height.value < adjacent(heightMap, point).map(_.value).min
}

val result = lowHeightsMap.values.map(h => basin(heightMap, h).size)

basin(heightMap, Height(5, Point(6, 4))).map(show(_)).mkString("\n")
