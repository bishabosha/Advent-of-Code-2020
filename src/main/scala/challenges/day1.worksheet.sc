import challenges._

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

def _n[T](f: Seq[Int] => Seq[T]) = (
  for
    ints <- io.unsafe.ints(challenge(day=1, part=0))
    res  <- f(ints.to(LazyList)).headOption.toRight("expected at least 1 result")
  yield
    res
).eval

val _0 = _n(ints => pairs(ints, ints))
val _1 = _n(ints => triples(ints, ints, ints))
