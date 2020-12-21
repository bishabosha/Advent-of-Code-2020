import challenges._

import collectionutil._

def parse(str: String) =
  Numeric[Long].parseString(str).toRight(s"not a number: $str")

lazy val _n =
  for
    lines <- io.unsafe.lineStream(challenge(day=9, part=0))
    longs <- traverse(lines)(parse)
  yield
    longs

val _1 = _n.eval
