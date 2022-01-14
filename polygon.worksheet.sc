val f0 = List(3,4,3,1,2)
val f1 = f0.map(_ - 1)
val f2 = f1.map(_ - 1)
f2.count(_ == -1)
f2.map(f => if (f == -1) 6 else f) ++ List.fill(3)(8)

List.fill(9)(0)