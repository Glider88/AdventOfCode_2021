import Helpers._

object Day4 extends App {
  val sample = """
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
"""

  type Row = Set[Int]
  type Column = Set[Int]
  type Board = (List[Row], List[Column])

  def numbers(source: String): List[Int] =
    source.trim.linesIterator.next().split(",").map(_.toInt).toList
  
  def asList(source: String): List[List[List[Int]]] = {
    def rowFormatted(row: String): List[Int] =
      row.trim.split("\\s+").map(_.toInt).toList

    def boardFormatted(board: String): List[List[Int]] =
      board.trim.linesIterator.map(rowFormatted(_)).toList     

    val boardsSource = "(?m)^$".r.split(source.trim).drop(1)
    boardsSource.map(boardFormatted(_)).toList
  }
  
  def boardWithoutNumber(number: Int, board: Board): Board = 
    (board._1.map(_ - number), board._2.map(_ - number))

  def hasEmptySet(sets: List[Set[Int]]): Boolean =
    !sets.filter(_.isEmpty).isEmpty
  
  def isWinBoard(board: Board): Boolean =
    hasEmptySet(board._1) || hasEmptySet(board._2)

  def isWin(boards: List[Board]): Boolean =
    !boards.filter(isWinBoard(_)).isEmpty

  def formatted(source: String): List[Board] = {
    val boardsList = asList(source)
    boardsList.map(rowsList => {
      val rows = rowsList.map(rowList => rowList.toSet)
      val columns = rowsList.transpose.map(columnList => columnList.toSet)
      (rows, columns)
    })
  }

  case class WinState(lastNumber: Int, winBoard: Board)

  def wonBoard(numbers: List[Int], boards: List[Board]): WinState = {
    val number :: tailNumbers = numbers
    val markedBoards = boards.map(boardWithoutNumber(number, _))
    if (isWin(markedBoards)) {
      WinState(number, markedBoards.filter(isWinBoard(_)).head)
    } else {
      wonBoard(tailNumbers, markedBoards)
    }
  }

  def part1(source: String): Unit = {
    val state = wonBoard(numbers(source), formatted(source))
    val sumNotMarked = state.winBoard._1.flatten.sum
    printGreen(s"${state.lastNumber} x $sumNotMarked = ${state.lastNumber * sumNotMarked}")
  }

  val source = read("day4") 

  part1(sample)
  println("---")
  part1(source)

  // println("---------------")

  // part2(sampleInput)
  // println("---")
  // part2(input)
}