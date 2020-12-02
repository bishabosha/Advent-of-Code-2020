package challenges
package day1

def pairs(xs: Seq[Int], ys: Seq[Int]): LazyList[((Int, Int), Int)] =
  for
    x <- xs.to(LazyList)
    y <- ys.to(LazyList)
    if x + y == 2020
  yield
    (x, y) -> x * y

def triples(xs: Seq[Int], ys: Seq[Int], zs: Seq[Int]): LazyList[((Int, Int, Int), Int)] =
  for
    x <- xs.to(LazyList)
    y <- ys.to(LazyList)
    z <- zs.to(LazyList)
    if x + y + z == 2020
  yield
    (x, y, z) -> x * y * z

def _0 = (
  for
    ints <- io.unsafe.ints(challenge(day=1, part=0))
  yield
    pairs(ints, ints).headOption
).eval

def _1 = (
  for
    ints <- io.unsafe.ints(challenge(day=1, part=0))
  yield
    triples(ints, ints, ints).headOption
).eval
