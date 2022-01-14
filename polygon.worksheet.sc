    def range(a: Int, b: Int): List[Int] =
      if (a <= b) (a to b).toList
      else (b to a).toList.reverse 


range(1, 4)
range(4, 1)


range(1, 4) zip range(4, 1)