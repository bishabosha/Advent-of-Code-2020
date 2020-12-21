import challenges._

import collectionutil._

val Entry = raw"(\d+)-(\d+) ([a-z]): (\w+)".r

type Policy = (Int, Int, Char, String) => Either[String, Boolean]

def validateOld(lo: Int, hi: Int, char: Char, password: String) =
     required(lo <= hi)
  %> (lo to hi).contains(password.count(_ == char))

def validateNew(lo: Int, hi: Int, char: Char, password: String) =
     required(lo <= hi && hi <= password.length)
  %> Seq(lo-1, hi-1).map(password(_) == char).reduce(_ ^ _)

def parseEntry(policy: Policy)(entry: String) = entry match
  case Entry(l, u, c, p) =>
    policy(l.toInt, u.toInt, c(0), p)
  case line =>
    Left(s"$line is not a valid entry, expected $Entry")

def _n(policy: Policy) =
  for
    lines   <- io.unsafe.lineStream(challenge(day=2, part=0))
    matches <- traverse(lines)(parseEntry(policy))
  yield
    matches.count(identity)

val _0 = _n(validateOld).eval
val _1 = _n(validateNew).eval
