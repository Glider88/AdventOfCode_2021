import collection.immutable.SortedMap
import Helpers._

object Day2 extends App {
  val sample = """
forward 5
down 5
forward 8
up 3
down 8
forward 2
"""

  sealed trait Direction
  case class Forward(value: Int) extends Direction
  case class Up(value: Int) extends Direction
  case class Down(value: Int) extends Direction

  def formatted(source: String): List[Direction] = {
    def value(line: String): Int = line.split(" ")(1).toInt
    source
      .linesIterator
      .map(_.trim).filterNot(_.isEmpty)
      .map {
        case u if u.startsWith("up") => Up(value(u))
        case d if d.startsWith("down") => Down(value(d))
        case f if f.startsWith("forward") => Forward(value(f))
        case e => throw new IllegalArgumentException(s"incorrect line $e")
      }
      .toList
  }

  def part1(commands: List[Direction]): Unit = {
    val path = commands.map {
      case Forward(value) => value
      case _ => 0
    }.sum

    val depth = commands.map {
      case Up(value) => -value
      case Down(value) => value
      case _ => 0
    }.sum

    printGreen(s"$path x $depth = ${path * depth}")
  }

  def part2(commands: List[Direction]): Unit = {
    var aim = 0
    var path = 0
    var depth = 0

    for (command <- commands) {
      command match {
        case Forward(f) => {
          path += f 
          depth += aim * f
        } 
        case Down(d) => {
          aim += d
        }
        case Up(u) => {
          aim -= u
        }
      }
    }

    printGreen(s"$path x $depth = ${path * depth}")
  }

  val sampleCommands = formatted(sample)
  val commands = formatted(read("day2"))

  part1(sampleCommands)
  println("---")
  part1(commands)
  println("----------------") 
  part2(sampleCommands)
  println("---")
  part2(commands)
}
