package io.unsafe

type IO[+A] = Either[java.io.IOException, A]
type Result[+E, +A] = Either[E, A]

def lines(file: os.Path): IO[IndexedSeq[String]] =
  try Right(os.read.lines(file))
  catch
    case err: java.io.IOException => Left(err)

def ints(file: os.Path): Result[java.io.IOException | NumberFormatException, IndexedSeq[Int]] =
  try lines(file).map(_.map(_.toInt))
  catch
    case err: NumberFormatException => Left(err)
