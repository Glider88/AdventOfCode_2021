import Helpers._
import io.AnsiColor._

object Day1 extends App {
  // 199  A      
  // 200  A B    
  // 208  A B C  
  // 210    B C D
  // 200  E   C D
  // 207  E F   D
  // 240  E F G  
  // 269    F G H
  // 260      G H
  // 263        H

  val sample = """
199
200
208
210
200
207
240
269
260
263
"""

  def formatted(source: String): List[Int] =
    source
      .linesIterator
      .map(_.trim).filterNot(_.isEmpty)
      .map(_.toInt)
      .toList

  def increased(depths: List[Int]): List[Int] =
    depths
      .init.zip(depths.tail)
      .map(p => p._2 > p._1)
      .map(b => if (b == true) 1 else 0)

  def part1(depths: List[Int]): Unit = {
    val increasedList = increased(depths) 
    val result = increasedList.sum
    printGreen(result.toString)
  }
 
  def part2(input: List[Int]): Unit = {
    val depths = input.sliding(3).map(_.sum).toList
    val increasedList = increased(depths) 

    val result = increasedList.sum
    printGreen(result.toString)
  }
  
  val sampleInput = formatted(sample)
  val input = formatted(read("day1"))

  part1(sampleInput)
  println("---")
  part1(input)

  println("---------------")

  part2(sampleInput)
  println("---")
  part2(input)
}
