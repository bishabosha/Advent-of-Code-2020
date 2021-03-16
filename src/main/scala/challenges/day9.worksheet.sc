import challenges._

import collectionutil._

def parse(str: String) =
  Numeric[Long].parseString(str).toRight(s"not a number: $str")

def zipper(preamble: Int)(seq: IndexedSeq[Long]) =
  ???

lazy val _n =
  for
    lines <- io.unsafe.lineStream(challenge(day=9, part=1))
    longs <- traverse(lines)(parse)
  yield
    longs

val _1 = _n.eval
