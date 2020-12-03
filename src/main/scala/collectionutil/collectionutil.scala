package collectionutil

import collection.mutable

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
