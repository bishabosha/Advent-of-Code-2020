package io.unsafe

type IO[+A] = Either[java.io.IOException, A]
type Result[+E, +A] = Either[E, A]

def lines(file: os.Path => os.Path): IO[IndexedSeq[String]] =
  try Right(os.read.lines(file(os.pwd)))
  catch
    case err: java.io.IOException => Left(err)
