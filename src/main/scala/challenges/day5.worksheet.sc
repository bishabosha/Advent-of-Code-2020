import challenges.*

import collectionutil.*

type RSplit = Range => Range
type Split[T] = T => RSplit
type Row = 'F' | 'B'
type Seat = 'R' | 'L'
type Position = (Seq[Row], Seq[Seat])

lazy val Lower: RSplit = r => r.head to ((r.head + r.last) / 2)
lazy val Upper: RSplit = r => (r.head + r.last + 1) / 2 to r.last

lazy val RowSplit: Split[Row] = {
  case 'F' => Lower
  case 'B' => Upper
}
lazy val SeatSplit: Split[Seat] = {
  case 'R' => Upper
  case 'L' => Lower
}

val Rows = 0 to 127
val Seats = 0 to 7

val Position = raw"([FB]{7})([LR]{3})".r

def parse(lines: geny.Generator[String]): Either[String, Seq[Position]] = traverse(lines) {
  case Position(row, seat) => Right((row.toIndexedSeq -> seat.toIndexedSeq).asInstanceOf[Position])
  case _                   => Left(s"expected $Position")
}

def fold[T](ap: Split[T], r: Range)(st: Seq[T]) = st.map(ap).reduce(_ andThen _)(r).ensuring(_.length == 1).head

def decodeId(sr: Split[Row], rows: Range, ss: Split[Seat], seats: Range)(p: Position): Int =
  val r = fold(sr, rows)(p(0))
  val s = fold(ss, seats)(p(1))
  r * 8 + s

def missing(ids: Seq[Int]): Int =
  val maxId = ids.max
  val minId = ids.min

  val allIds =
    (for
      r <- Rows
      s <- Seats
    yield r * 8 + s).filter((minId to maxId).contains).toSet

  (allIds -- ids).ensuring(_.size == 1).head

lazy val _n =
  for
    lines <- io.unsafe.lineStream(challenge(day = 5, part = 0))
    pss <- parse(lines)
  yield pss.map(decodeId(RowSplit, Rows, SeatSplit, Seats))

val _0 = _n.map(_.max).eval
val _1 = _n.map(missing).eval
