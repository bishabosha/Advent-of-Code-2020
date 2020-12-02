package collectionutil

import collection.mutable

def traverse[E,T,U](ts: List[T])(f: T => Either[E, U]): Either[E, List[U]] =

  def inner(buf: mutable.ListBuffer[U], ts: List[T]): Either[E, List[U]] = ts match
    case Nil     => Right(buf.toList)
    case t :: ts =>
      f(t) match
        case Left(e)  => Left(e)
        case Right(b) => inner(buf += b, ts)

  inner(mutable.ListBuffer.empty, ts)

object Validate:

  inline def required(inline test: Boolean) =
    val valid = test
    if test then Right(test)
    else Left("requirement failed: " + compiletime.codeOf(test))

  extension [E](e: Either[E, Boolean]):
    inline def && (inline that: Boolean) = e match
      case err @ Left(_) => err
      case Right(acc) => Right(acc && that)
