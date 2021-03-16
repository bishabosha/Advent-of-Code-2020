package collectionutil

import collection.mutable

type State[S, T] = S => (T, S)

def traverseState[S, T, U](tss: Seq[State[S, T]]): State[S, Seq[T]] = { s =>
  val (buf, s1) = tss.foldLeft((tss.iterableFactory.newBuilder[T], s)) { (acc, ts) =>
    val (z, s) = acc
    map(ts)(z += _)(s)
  }
  (buf.result, s1)
}

extension [S, T, U, V](st: State[S, T]) def flatMap(f: T => State[S, U]): State[S, U] = { s =>
  val (t, s1) = st(s)
  f(t)(s1)
}

extension [S, T, U, V](st: State[S, T]) def map(f: T => U): State[S, U] = { s =>
  val (t, s1) = st(s)
  (f(t), s1)
}

def occurrences[T](ts: Seq[T]) = ts.groupBy(identity).view.mapValues(_.length)

def groupLines[C[X] <: Seq[X]](lines: C[String])(using fac: collection.Factory[Seq[String], C[C[String]]]): C[C[String]] =

  def inner(acc: mutable.Builder[Seq[String], C[C[String]]], remaining: Seq[String]): C[C[String]] =
    val (group, remaining1) = remaining.dropWhile(_.trim.isEmpty).span(_.trim.nonEmpty)
    acc += group
    if remaining1.isEmpty then
      acc.result
    else
      inner(acc, remaining1)

  inner(fac.newBuilder, lines)

def traverse[E,T,U,C[X] <: Seq[X]](ts: C[T])(f: T => Either[E, U])(using fac: collection.Factory[U, C[U]]): Either[E, C[U]] =

  def inner(buf: mutable.Builder[U, C[U]], ts: Iterator[T]): Either[E, C[U]] =
    if ts.hasNext then
      f(ts.next) match
        case Left(e)  => Left(e)
        case Right(b) => inner(buf += b, ts)
    else
      Right(buf.result)

  inner(fac.newBuilder, ts.iterator)

def traverse[E,T,U: reflect.ClassTag](ts: geny.Generator[T])(f: T => Either[E, U]): Either[E, IndexedSeq[U]] =
  val buf = mutable.ArrayBuilder.make[U]
  var resErr: Left[E, Nothing] | Null = null
  val access = ts.generate { t =>
    f(t) match
      case Right(u) =>
        buf += u
        geny.Generator.Continue
      case Left(err) =>
        resErr = Left(err)
        geny.Generator.End

  }
  if resErr eq null then Right(buf.result.toIndexedSeq)
  else resErr

inline def required(inline test: Boolean) =
  if test then Right(())
  else Left("requirement failed: " + compiletime.codeOf(test))

extension [E, T, U](e: Either[E, T])
  def %> (that: => U): Either[E, U] = e.map(_ => that)

  def *> (that: => Either[E,U]): Either[E, U] = e.flatMap(_ => that)

  def !> (f: T => U): Either[E, U] = e.map(f)

extension [T](p: T => Boolean)
  def && (q: T => Boolean): T => Boolean = t => p(t) && q(t)
