import Helpers._
import scala.math.abs

object Day7 extends App {
  val sample = "16,1,2,0,4,2,7,1,2,14"

  def formatted(source: String): List[Int] = source.trim.split(",").map(_.trim.toInt).toList 

  def part1(input: List[Int]): Unit = {
    def fuelCrabs(crabs: List[Int])(center: Int) = crabs.map(c => abs(c - center)).sum
    val fuel = fuelCrabs(input)
    val minFuel = input.map(fuel(_)).min
    printGreen(s"${minFuel}")
  }
  
  def part2(input: List[Int]): Unit = {
    val ints: Stream[Int] = {
      def loop(v: Int): Stream[Int] = v #:: loop(v + 1)
      loop(1)
    }

    val range = (input.min to input.max).toList
    val sums = range.map(i => (i, ints.take(i).sum)).toMap
    def fuelCrabs(crabs: List[Int])(center: Int) = crabs.map(c => sums(abs(c - center))).sum
    val fuel = fuelCrabs(input)
    val minFuel = range.map(a => fuel(a)).min
    
    printGreen(s"${minFuel}")
  }

  val source = read("day7")

  part1(formatted(sample))
  println("---")
  part1(formatted(source))

  println("---------------")

  part2(formatted(sample))
  println("---")
  part2(formatted(source))
}
