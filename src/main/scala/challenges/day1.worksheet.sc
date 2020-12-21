import challenges._

import collectionutil._

def pairs(xs: Seq[Int], ys: Seq[Int]): Seq[Int] =
  for
    x <- xs
    y <- ys
    if x != y && x + y == 2020
  yield
    x * y

def triples(xs: Seq[Int], ys: Seq[Int], zs: Seq[Int]): Seq[Int] =
  for
    x <- xs
    y <- ys
    z <- zs
    if x != y && x != z && y != z && x + y + z == 2020
  yield
    x * y * z

def parseInt(str: String) =
  Numeric[Int].parseString(str).toRight(s"not a number: $str")

def _n[T](f: Seq[Int] => Seq[T]) = (
  for
    lines <- io.unsafe.lineStream(challenge(day=1, part=0))
    ints  <- traverse(lines)(parseInt)
    res   <- f(ints.to(LazyList)).headOption.toRight("expected at least 1 result")
  yield
    res
)

val _0 = _n(ints => pairs(ints, ints)).eval
val _1 = _n(ints => triples(ints, ints, ints)).eval
