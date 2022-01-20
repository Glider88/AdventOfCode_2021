import Helpers._

object Day11 extends App {
  val sample = """
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
"""

  case class Point(x: Int, y: Int)
  case class Octopus(energy: Int, point: Point)
  type OctopusMap = Map[Point, Octopus]

  def formatted(source: String): OctopusMap = {
    val octopusLists = source.trim.linesIterator.toList.map(_.toList.map(_.toString.toInt))

    octopusLists.zipWithIndex.map {
      case (row, columnIndex) => row.zipWithIndex.map {
        case (energy, rowIndex) => {
          val point = Point(rowIndex, columnIndex)
          (point -> Octopus(energy, point))
        }
      }
    }.flatten.toMap
  }

  def incrementEnergy(octopus: Octopus): Octopus = octopus.copy(energy = octopus.energy + 1)
  def resetEnergy(octopus: Octopus): Octopus = octopus.copy(energy = 0)

  def accumulated(octopus: Octopus): Octopus = incrementEnergy(octopus)
  def absorbed(octopus: Octopus): Octopus =
    if (octopus.energy != 0) incrementEnergy(octopus)
    else octopus 

  def accumulated(octopuses: OctopusMap): OctopusMap = octopuses.map(_ -> accumulated(_))
  def absorbed(octopuses: OctopusMap): OctopusMap = octopuses.map(_ -> absorbed(_))

  def adjacent(octopuses: OctopusMap, center: Point): OctopusMap = {
    val rawOption = for {
      dx <- -1 to 1
      dy <- -1 to 1
    } yield octopuses.get(Point(center.x + dx, center.y + dy))

    val raw = rawOption.filter(_.isDefined).map(_.get)
    raw.map(o => o.point -> o).toMap
  }

  def flashed(octopuses: OctopusMap): OctopusMap = {
    val ready = octopuses.filter {
      case (_, o) => o.energy > 9 && o.energy != 0
    }

    if (ready.isEmpty) {
      octopuses
    } else {
      val highlighted = ready.map {
        case (p, o) => p -> resetEnergy(o)
      }

      val next = highlighted.foldLeft(octopuses)((acc, po) => {
        acc ++ absorbed(adjacent(acc, po._1))
      })

      flashed(next ++ highlighted) 
    }
  }

  def highlighted(octopuses: OctopusMap): Int =
    octopuses.filter {
      case (_, o) => o.energy == 0
    }.size
 

  def part1(octopuses: OctopusMap): Unit = {
    val steps = (1 to 100).foldLeft(List(octopuses))((octopusesList, _) => {
      octopusesList :+ flashed(accumulated(octopusesList.last))
    })

    val result = steps.tail.map(highlighted(_)).sum

    printGreen(s"$result")
  }

  def part2(octopuses: OctopusMap): Unit = {
    var step = 0
    var state = octopuses
    while (highlighted(state) != 100) {
      state = flashed(accumulated(state))
      step += 1
    }

    printGreen(s"$step")
  }

  val source = read("day11")

  part1(formatted(sample))
  println("---")
  part1(formatted(source))

  println("---------------")

  part2(formatted(sample))
  println("---")
  part2(formatted(source))
}
