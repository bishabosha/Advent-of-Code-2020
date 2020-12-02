package challenges
package day2

import collectionutil.Validate._

val Entry = raw"(\d+)-(\d+) ([a-z]): (\w+)".r

def validateOld(lo: Int, hi: Int, char: Char, password: String) =
  required(lo <= hi) && (lo to hi).contains(password.count(_ == char))

def validateNew(lo: Int, hi: Int, char: Char, password: String) =
  required(lo <= hi & hi <= password.length)
  && (password(lo - 1) == char ^ password(hi - 1) == char)

def parseEntry(policy: (Int, Int, Char, String) => Either[String, Boolean])(entry: String) = entry match
  case Entry(l, u, c, p) =>
    policy(l.toInt, u.toInt, c(0), p)
  case line =>
    Left(s"$line is not a valid entry, expected $Entry")

def _n(policy: (Int, Int, Char, String) => Either[String, Boolean]) =
  for
    lines   <- io.unsafe.lines(challenge(day=2, part=0))
    matches <- collectionutil.traverse(lines.toList)(parseEntry(policy))
  yield
    matches.count(x => x)

def _0 = _n(validateOld).eval
def _1 = _n(validateNew).eval
