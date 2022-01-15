def foo(i: List[Int])(n: Int) = i.map(j => scala.math.abs(j - n)).sum
val f = foo(List(16,1,2,0,4,2,7,1,2,14))(_)


f(2)
f(1)
f(3)
f(10)


val ints: Stream[Int] = {
  def loop(v: Int): Stream[Int] = v #:: loop(v + 1)
  loop(1)
}

ints.take(3).sum




val l = List(16,1,2,0,4,2,7,1,2,14)
def fuelCrabs(crabs: List[Int])(center: Int) = crabs.map(c => ints.take(math.abs(c - center)).sum).sum

fuelCrabs(l)(5)


