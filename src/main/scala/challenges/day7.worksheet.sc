import challenges.*

import collectionutil._

import cats.parse.{Parser => P, Numbers}

case class NestedBag(bag: Int, count: Int)
case class Bag(bag: Int, rules: Seq[NestedBag])

def ruleParser[S](encode: ((String, String)) => State[S, Int]) =

  val sp = P.char(' ')

  val word = P.charsWhile(('a' to 'z').contains)

  val nat = (Numbers.nonZeroDigit ~ Numbers.digits0).map((s, r) => s"$s$r".toInt)

  val one = P.char('1')

  val adjColor = (word <* sp) ~ (word <* sp)

  val noBags = P.string("no other bags").as(List.empty)

  val oneBag = ((one <* sp) *> adjColor <* P.string("bag")).map(
    encode(_).map(NestedBag(_, count = 1)))

  val multiBag = ((nat <* sp) ~ adjColor <* P.string("bags")).map((n, p) =>
    encode(p).map(NestedBag(_, count = n)))

  val natBag = P.oneOf(oneBag :: multiBag :: Nil)

  val natBags = P.repSep(natBag, min = 1, sep = P.string(", "))

  val nestedBags = P.oneOf(noBags :: natBags.map(_.toList) :: Nil) <* P.char('.') map traverseState

  val bag = (adjColor <* P.string("bags contain ")).map(encode)

  (bag ~ nestedBags <* P.end).map((bs, rss) =>
    for
      b  <- bs
      rs <- rss
    yield Bag(b, rs))

def parse[T](parser: P[T])(line: String) =
  parser.parse(line).map(_(1))

type Encoder = Map[(String, String), Int]
type Lookup = IArray[Seq[NestedBag]]

def uniqueId(p: (String, String)): State[(Int, Encoder), Int] = { s =>
  val (maxId, encoder) = s
  encoder.get(p) match
    case Some(i) => (i, s)
    case None    => (maxId, (maxId + 1, encoder.updated(p, maxId)))
}

def process(rs: Seq[Bag], size: Int) =
  val ar: Array[Seq[NestedBag]] = new Array(size)
  for r <- rs do
    ar(r.bag) = r.rules
  IArray.unsafeFromArray(ar)

def op[T](f: (Lookup, Int) => T)(adj: String, color: String)(rs: Seq[Bag], s: (Int, Encoder)): Either[String, T] =
  val (maxId, encoder) = s
  val bags = process(rs, maxId)
  for
    id <- encoder.get((adj, color)).toRight(s"unknown bag $adj $color")
  yield
    f(bags, id)

def countContainers =
  op((bags, id) => bags.indices.count(bag => containsPath(bags, id, bag)))

def countNested =
  op(nestedSize)

def containsPath(bags: Lookup, target: Int, bag: Int): Boolean =
  bags(bag).exists(r => r.bag == target || containsPath(bags, target, r.bag))

def nestedSize(bags: Lookup, bag: Int): Int =
  bags(bag).map(r => r.count + r.count * nestedSize(bags, r.bag)).sum

lazy val _n =
  for
    lines <- io.unsafe.lineStream(challenge(day=7, part=0))
    rules <- traverse(lines)(parse(ruleParser(uniqueId)))
  yield
    traverseState(rules)((0, Map()))

val _0 = _n.flatMap(countContainers("shiny", "gold").tupled).eval
val _1 = _n.flatMap(countNested("shiny", "gold").tupled).eval
