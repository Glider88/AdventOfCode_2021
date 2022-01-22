val ps = List((1, 2), (3, 1), (2, 3))

val o = 1
val t = 2

ps.map {
  case (`o`, v) => s"$o-$v"
  case (w, `o`) => s"$w-$o"
  case _ => "?" 
}

val l = List("a", "b", "b", "c") diff List("b") diff List("b")

Set("a", "b", "c") intersect List("b", "b", "c").toSet

val (b, s) = Set(1, 3, 2, 4).partition(_ > 2)

List(1, 2, 3, 1).distinct
