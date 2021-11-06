import challenges.*

import collectionutil.*

def anyYes(l: Seq[Seq[String]]) =
  l.map(_.flatten.toSet.size).sum

def allYes(l: Seq[Seq[String]]) =
  l.map(g => occurrences(g.flatten).count((_, s) => s == g.length)).sum

lazy val _n =
  for lines <- io.unsafe.lines(challenge(day = 6, part = 0))
  yield groupLines[Seq](lines)

val _0 = _n.map(anyYes).eval
val _1 = _n.map(allYes).eval
