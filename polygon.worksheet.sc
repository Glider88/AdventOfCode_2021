  val sample = """
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
"""

val p :: f :: Nil = "(?m)^$".r.split(sample.trim).toList

def range(a: Int, b: Int): List[Int] =
  if (a <= b) (a to b).toList
  else (b to a).toList.reverse 

(range(7 + 1, 7 * 2) zip range(7 - 1, 0)).toMap
/*
     0 1 2 3 4 5 6 7 8 9 10
   0 . . . # . | # . . #  .
   1 . . . . # | . . . .  .
   2 . . . . . | . . . .  . 
   3 # . . . . | . . . .  . 
   4 . . . # . | . . # .  # 
   5 . . . . . | . . . .  . 
   6 . . . . . | . . . .  . 
   7 - - - - - + - - - -  - 
   8 . . . . . | . . . .  . 
   9 . . . . . | . . . .  . 
  10 . # . . . | # . # #  .
  11 . . . . # | . . . .  .
  12 . . . . . | # . . .  # 
  13 # . . . . | . . . .  . 
  14 # . # . . | . . . .  .

  6 ->  8
  5 ->  9
  4 -> 10
  3 -> 11
  2 -> 12
  1 -> 13
  0 -> 14

  4 ->  6
  3 ->  7
  2 ->  8
  1 ->  9
  0 -> 10
*/