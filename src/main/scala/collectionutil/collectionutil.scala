package collectionutil

import collection.mutable

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

inline def required(inline test: Boolean) =
  if test then Right(())
  else Left("requirement failed: " + compiletime.codeOf(test))

extension [E, T, U](e: Either[E, T]):
  def %> (that: => U): Either[E, U] = e.map(_ => that)

  def *> (that: => Either[E,U]): Either[E, U] = e.flatMap(_ => that)

  def !> (f: T => U): Either[E, U] = e.map(f)

extension [T](p: T => Boolean):
  def && (q: T => Boolean): T => Boolean = t => p(t) && q(t)
