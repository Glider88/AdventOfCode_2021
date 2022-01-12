import Helpers._
import scala.collection.mutable.ListBuffer

object Day3 extends App {
  val sample = """
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
"""
  def charToInt(c: Char): Int = c.toString.toInt
  def formatted(source: String): List[String] =
    source
      .linesIterator
      .map(_.trim).filterNot(_.isEmpty)
      .toList

  def column(matrix: List[String], columnIndex: Int): List[Int] =
    matrix.map(row => charToInt(row(columnIndex)))

  def mostCommonBit(bits: List[Int]): Option[Int] = {
    val sum = bits.sum
    val half = bits.size / 2.0
    
    if (sum == half) {
      None
    } else {
      if (sum > half) Some(1) else Some(0)
    }
  }

  def reversedBit(bit: Int): Int =
    if (bit == 0) 1 else 0
  
  def reversedBits(bits: List[Int]): List[Int] =
    bits.map(reversedBit(_))

  def toDec(bits: List[Int]): Int =
    Integer.parseInt(bits.mkString, 2)

  def toDec(bits: String): Int =
    Integer.parseInt(bits, 2)

  def part1(matrix: List[String]): Unit = {
    val height = matrix.size
    val width = matrix.head.size

    val transposed = (0 until width).map(column(matrix, _)).toList
    val gammaBinary = transposed.map(row =>
      mostCommonBit(row) match {
        case Some(value) => value
        case None => throw new RuntimeException(s"Input data error, for column $row")
      }
    )
    val epsilonBinary = reversedBits(gammaBinary)

    val gamma = toDec(gammaBinary)
    val epsilon = toDec(epsilonBinary)

    printGreen(s"$gamma x $epsilon = ${gamma * epsilon}")
  }

  def part2(matrix: List[String]): Unit = {
    val height = matrix.size
    val width = matrix.head.size

    def oxygenRating(remainder: List[String], columns: List[Int]): Int = {
      columns match {
        case Nil => throw new RuntimeException(s"Invalid oxygen input data, empty columns, remainder: $remainder")
        case i :: cols => {
          val mcb = mostCommonBit(column(remainder, i)) match {
            case Some(value) => value 
            case None => 1
          } 
          val newRemainder = remainder.filter(row => charToInt(row(i)) == mcb)
          newRemainder match {
            case Nil => throw new RuntimeException(s"Invalid oxygen input data, empty new remainder, remainder: $remainder")
            case result :: Nil => toDec(result)
            case nr => oxygenRating(nr, cols) 
          }
        }
      }
    }

    def co2Rating(remainder: List[String], columns: List[Int]): Int = {
      columns match {
        case Nil => throw new RuntimeException(s"Invalid co2 input data, empty columns, remainder: $remainder")
        case i :: cols => {
          val lcb = mostCommonBit(column(remainder, i)) match {
            case Some(value) => reversedBit(value)
            case None => 0
          } 
          val newRemainder = remainder.filter(row => charToInt(row(i)) == lcb)
          newRemainder match {
            case Nil => throw new RuntimeException(s"Invalid co2 input data, empty new remainder, remainder: $remainder")
            case result :: Nil => toDec(result)
            case nr => co2Rating(nr, cols) 
          }
        }
      }
    }

    val oxygen = oxygenRating(matrix, (0 until width).toList)
    val co2 = co2Rating(matrix, (0 until width).toList)

    printGreen(s"$oxygen x $co2 = ${oxygen * co2}")
  }

  val sampleReport = formatted(sample)
  val report = formatted(read("day3"))

  part1(sampleReport)
  println("---")
  part1(report)
  println("----------------") 
  part2(sampleReport)
  println("---")
  part2(report)
}

