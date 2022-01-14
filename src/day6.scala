
import Helpers._

object Day6 extends App {
  val sample = "3,4,3,1,2"

  def formatted(source: String): List[Int] = source.split(",").map(_.toInt).toList

  def fishListV1(fishes: List[Int], lastDay: Int, day: Int = 1): List[Int] = {
    val nextFishesRaw = fishes.map(_ - 1)
    val births = nextFishesRaw.count(_ == -1)
    val nextFishes = nextFishesRaw.map(f => if (f == -1) 6 else f) ++ List.fill(births)(8)
    if (day == lastDay) {
      nextFishes
    } else {
      fishListV1(nextFishes, lastDay, day + 1)
    }
  }

  def fishListV2(initFishes: List[Int], lastDay: Int, firstDay: Int = 1): List[Int] = {
    var fishes = scala.collection.mutable.ArrayBuffer.empty[Int]
    fishes ++= initFishes
    var day = firstDay

    while (day <= lastDay) {
      var number = 0
      var born = 0
      var size = fishes.size
      while (number < size) {
        var fish = fishes(number)
        fish -= 1
        if (fish == -1) {
          fishes(number) = 6
          born += 1
        } else {
          fishes(number) = fish
        }
        number += 1
      }

      fishes ++= List.fill(born)(8)
      day += 1
    }

    fishes.toList
  }
  
  def fishListV3(fishes: List[Long], lastDay: Int, day: Int = 1): List[Long] = {
    val zero :: tail = fishes
    val nextRaw = tail :+ zero
    val nextFishList = nextRaw.updated(6, nextRaw(6) + zero)
    if (day == lastDay) {
      nextFishList
    } else {
      fishListV3(nextFishList, lastDay, day + 1)
    }
  }

  def part1(fishes: List[Int]): Unit = {
    val finalFishes = fishListV1(fishes, 80) 
    printGreen(s"${finalFishes.size}")
  }
  
  def part2(fishes: List[Int]): Unit = {
    val fish = fishes.foldLeft(List.fill(9)(0L))((f, n) => {
      f.updated(n, f(n) + 1L)
    })
    val result = fishListV3(fish, 256) 
    printGreen(s"${result.sum}")
  }

  val source = read("day6")

  part1(formatted(sample))
  println("---")
  part1(formatted(source))

  println("---------------")

  part2(formatted(sample))
  println("---")
  part2(formatted(source))
}
