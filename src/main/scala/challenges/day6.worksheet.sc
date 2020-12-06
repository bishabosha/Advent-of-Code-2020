import challenges._

import collectionutil._

def _n[T] =
  for
    lines <- io.unsafe.lines(challenge(day=6, part=0))
  yield
    groupLines[Seq](lines)

val _1 = _n.eval
