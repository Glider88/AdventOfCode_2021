import java.util.function.Predicate
val sample = """
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
"""


val m1 = Map(1 -> Set('a'))
val m2 = Map(2 -> Set('b'))
val m3 = Map(1 -> Set('c'))

m1 ++ m2 ++ m3

type Chars = Set[Char]

val line = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

val left = line.split("\\|").apply(0).trim
val right = line.split("\\|").apply(1).trim

List(1, 2, 3) :+ 4

val target: List[Set[Char]] =
  left.split(" ").toList.map(_.toSet)

val info = List(
  Set('c', 'f'),
  Set('a', 'c', 'f'),
  Set('b', 'c', 'd', 'f'),
  Set('a', 'b', 'c', 'd', 'e', 'f', 'g')
)

def cartesian[A](xs: List[A], ys: List[A]): List[(A, A)] =
  for { x <- xs; y <- ys } yield (x, y)

Set('a', 'd', 'b') diff Set('a', 'b')
Set('a', 'b') diff Set('a', 'd', 'b') 

Set('a', 'd', 'b') intersect Set('a', 'b')
Set('a', 'b') intersect Set('a', 'd', 'b') 
 
case class Translate(unknown: Chars, known: Chars)

val targetMap = target.map(w => (w.size, w)).toMap
val infoMap = info.map(w => (w.size -> w)).toMap

val determinedSizes = List(2, 3, 4, 7)

def diff(p1: Translate, p2: Translate): Option[Translate] = {
  val unknown = p1.unknown diff p2.unknown
  val known = p1.known diff p2.known

  if (unknown.nonEmpty && known.nonEmpty) {
    Some(Translate(unknown, known))
  } else {
    None
  }
}

val e = Set.empty[Char]
val s1 = Set('b', 'c')
val s2 = Set('a', 'b', 'c', 'd')
val s3 = Set('a', 'd')

diff(Translate(s1, s1), Translate(s2, s2))
diff(Translate(s2, s2), Translate(s1, s1))
diff(Translate(s3, s3), Translate(s1, s1))
diff(Translate(s3, s3), Translate(s3, s3))

def show(ps: List[Translate]): String =
  ps.map(t => s"${t.unknown.mkString} ${t.known.mkString}").mkString("\n")

def multiDiff(translate: Translate, diffList: List[Translate]): Option[Translate] =
  diffList.foldLeft(Option(translate))((acc, t2) => acc match {
    case Some(t1) => diff(t1, t2)
    case None => None 
  })

val s = Set('a', 'b')
val ss = Set('f', 'g')
val sss = Set('t')
val ssss = Set('a', 'f', 'b', 'g', 'z')

multiDiff(Translate(ssss, ssss), List(Translate(s, s), Translate(ss, ss), Translate(sss, sss)))


List(Some('a'), None, Some('b'), None, Some('c')).filter(_.isDefined).flatten

def pruned(predictions: List[Translate]): List[Translate] = {
  val first :: tail = predictions
  val raw = tail.foldLeft(List(Option(first)))(
    (acc, t) =>
      acc :+ multiDiff(t, acc.flatten)
  ) 
 
  raw.filter(_.isDefined).flatten
}

val initPredictions = determinedSizes.map(s => Translate(targetMap(s), infoMap(s)))

val dictionary = pruned(initPredictions)






val numberToCode = Map(
  0 -> Set('a', 'b', 'c', 'e', 'f', 'g'),
  1 -> Set('c', 'f'),
  2 -> Set('a', 'c', 'd', 'e', 'g'),
  3 -> Set('a', 'c', 'd', 'f', 'g'),
  4 -> Set('b', 'c', 'd', 'f'),
  5 -> Set('a', 'b', 'd', 'f', 'g'),
  6 -> Set('a', 'b', 'd', 'e', 'f', 'g'),
  7 -> Set('a', 'c', 'f'),
  8 -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
  9 -> Set('a', 'b', 'c', 'd', 'f', 'g')
)

val codeToNumber = for ((k,v) <- numberToCode) yield (v, k)

def trans(dictionary: List[Translate], word: Chars): Chars =
  dictionary.map(t => {
    if (t.unknown.subsetOf(word)) {
      t.known
    } else {
      Set.empty[Char]
    }
  })
  .flatten
  .toSet

trans(dictionary, "cdfeb".toSet)


val rs: List[Set[Char]] =
  right.split(" ").toList.map(_.toSet)

val ts = rs.map(r => {
  // val conditates = numberToCode.filter((kv) => kv._2.size == r.size) 
  val conditates = numberToCode.filter {
    case (_, code) => code.size == r.size
  } 
  val tr = conditates.filter((kv) => trans(dictionary, r).subsetOf(kv._2))
  if (tr.size != 1) {
    throw new RuntimeException("wrong")
  } else {
    tr.head._2
  }
})

ts.map(codeToNumber(_))