import challenges._

import collectionutil._

import cats.parse.Parser1

case class SubRule(bag: Int, count: Int)
case class Rule(bag: Int, rules: Seq[SubRule])

def ruleParser[S](encode: ((String, String)) => State[S, Int]) =
  import cats.parse.{Parser => P, Numbers}

  val sp = P.char(' ')

  val word = P.charsWhile1(('a' to 'z').contains)

  val nat = (Numbers.nonZeroDigit ~ Numbers.digits).map((s, r) => s"$s$r".toInt)

  val one = P.char('1')

  val adjColor = (word <* sp) ~ (word <* sp)

  val noRules = P.string1("no other bags").as(List.empty)

  val oneRule = ((one <* sp) *> adjColor <* P.string1("bag")).map(
    encode(_).map(SubRule(_, count = 1)))

  val multiRule = ((nat <* sp) ~ adjColor <* P.string1("bags")).map((n, p) =>
    encode(p).map(SubRule(_, count = n)))

  val natRule = P.oneOf1(oneRule :: multiRule :: Nil)

  val natRules = P.repSep(natRule, min = 1, sep = P.string1(", "))

  val subRules = P.oneOf(noRules :: natRules :: Nil).map(traverseState)

  val bag = (adjColor <* P.string1("bags contain ")).map(encode)

  (bag ~ subRules <* (P.char('.') <* P.end)).map((bs, rss) =>
    for
      b  <- bs
      rs <- rss
    yield Rule(b, rs))

def parse[T](parser: Parser1[T])(line: String) =
  parser.parse(line).map(_(1))

type Encoder = Map[(String, String), Int]
type Lookup = IArray[Seq[SubRule]]

def uniqueId(p: (String, String)): State[(Int, Encoder), Int] = { s =>
  val (maxId, encoder) = s
  encoder.get(p) match
    case Some(i) => (i, s)
    case None    => (maxId, (maxId + 1, encoder.updated(p, maxId)))
}

def process(rs: Seq[Rule], size: Int) =
  val ar: Array[Seq[SubRule]] = new Array(size)
  for r <- rs do
    ar(r.bag) = r.rules
  IArray.unsafeFromArray(ar)

def op[T](f: (Lookup, Int) => T)(adj: String, color: String)(rs: Seq[Rule], s: (Int, Encoder)): Either[String, T] =
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

def _n[T](f: (Seq[Rule], (Int, Encoder)) => T) =
  for
    lines <- io.unsafe.lines(challenge(day=7, part=0))
    rules <- traverse(lines)(parse(ruleParser(uniqueId)))
  yield
    f.tupled(traverseState(rules)((0, Map())))

val _0 = _n(countContainers("shiny", "gold")).flatten.eval
val _1 = _n(countNested("shiny", "gold")).flatten.eval
