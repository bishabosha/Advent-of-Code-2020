import challenges.*

import collectionutil.*

def parse(rows: IndexedSeq[String]): Either[String, Run] =
  required(rows.nonEmpty)
    *> required(rows.forall(_.length == rows.head.length))
    *> traverse(rows)(traverse(_) {
      case '#' => Right(1)
      case '.' => Right(0)
      case c   => Left(s"unknown char $c")
    })
    !> (grid => run(grid.length, (x, y) => grid(x)(y % grid.head.length)))

type Run = Slope => Int
type Slope = (Int, Int)

def run(height: Int, collision: (Int, Int) => Int)(r: Int, d: Int) =
  Iterator
    .from(1)
    .takeWhile(_ * d < height)
    .foldLeft(0)((acc, i) => acc + collision(i * d, i * r))

def slope(right: Int, down: Int) = (right, down)

def trees(ss: Slope*)(r: Run) = ss.map(BigInt.apply compose r).product

val s1 = slope(right = 3, down = 1)
val s2 = slope(right = 1, down = 1)
val s3 = slope(right = 5, down = 1)
val s4 = slope(right = 7, down = 1)
val s5 = slope(right = 1, down = 2)

lazy val _n =
  for
    lines <- io.unsafe.lines(challenge(day = 3, part = 0))
    run <- parse(lines)
  yield run

val _0 = _n.map(trees(s1)).eval
val _1 = _n.map(trees(s1, s2, s3, s4, s5)).eval
