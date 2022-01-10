object Helpers {
  def read(file: String): String = scala.io.Source.fromFile(s"resources/$file").mkString
  def printGreen(text: String): Unit = println(s"${Console.GREEN}$text${Console.RESET}")
}