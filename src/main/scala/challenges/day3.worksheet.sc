import challenges._

import collectionutil._

def grid(rows: IndexedSeq[String]): Either[String, Grid] =
      required(rows.nonEmpty)
  *>  required(rows.forall(_.length == rows.head.length))
  *>  traverse(rows)(traverse(_)(c =>
        if c == '#' then Right(true)
        else if c == '.' then Right(false)
        else Left(s"unknown char $c")
      ))
  !>  (grid => Grid(grid.length, (x, y) => grid(x)(y % grid.head.length)))

case class Grid(height: Int, isTree: (Int, Int) => Boolean)

type Policy = (Int, Grid) => Int => Int

def slope(r: Int, d: Int): Policy = (i, g) =>
  if i * d >= g.height || !g.isTree(i * d, i * r) then
    _ + 0
  else
    _ + 1

def collisions(p: Policy)(g: Grid) =
  (for i <- 1 until g.height yield p(i, g)).foldRight(0)(_(_))

def trees(ps: Policy*)(g: Grid) =
  ps.map(p => BigInt(collisions(p)(g))).product

val s1 = slope(r=3, d=1)
val s2 = slope(r=1, d=1)
val s3 = slope(r=5, d=1)
val s4 = slope(r=7, d=1)
val s5 = slope(r=1, d=2)

def _n[T](f: Grid => T) =
  for
    lines <- io.unsafe.lines(challenge(day=3, part=0))
    grid  <- grid(lines)
  yield
    f(grid)

val _0 = _n(trees(s1)).eval
val _1 = _n(trees(s1, s2, s3, s4, s5)).eval
