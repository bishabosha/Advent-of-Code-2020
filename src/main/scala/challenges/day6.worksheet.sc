import challenges._

import collectionutil._

def anyYes(l: Seq[Seq[String]]) =
  l.map(_.flatten.toSet.size).sum

def allYes(l: Seq[Seq[String]]) =
  l.map(g => occurrences(g.flatten).count((_, s) => s == g.length)).sum

def _n[T](f: Seq[Seq[String]] => T) =
  for
    lines <- io.unsafe.lines(challenge(day=6, part=0))
  yield
    f(groupLines[Seq](lines))

val _0 = _n(anyYes).eval
val _1 = _n(allYes).eval
