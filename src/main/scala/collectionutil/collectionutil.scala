package collectionutil

import collection.mutable

def traverse[E,T,U,C[X] <: Seq[X]](ts: C[T])(f: T => Either[E, U])(using fac: collection.Factory[U, C[U]]): Either[E, C[U]] =

  def inner(buf: mutable.Builder[U, C[U]], ts: Seq[T]): Either[E, C[U]] = ts match
    case t +: ts => f(t) match
      case Left(e)  => Left(e)
      case Right(b) => inner(buf += b, ts)
    case _       => Right(buf.result)

  inner(fac.newBuilder, ts)

inline def required(inline test: Boolean) =
  val valid = test
  if test then Right(())
  else Left("requirement failed: " + compiletime.codeOf(test))

extension [E, T, U](e: Either[E, T]):
  def %> (that: => U): Either[E, U] = e.map(_ => that)

  def *> (that: => Either[E,U]): Either[E, U] = e.flatMap(_ => that)

  def !> (f: T => U): Either[E, U] = e.map(f)
