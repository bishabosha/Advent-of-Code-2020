import challenges._

import collectionutil._

case class Bag(adj: String, color: String)
case class SubRule(count: Int, bag: Bag)
case class Rule(s: Bag, rules: Seq[SubRule])

val rule =
  import cats.parse.{Parser => P, Numbers, Parser1}

  val sp = P.char(' ')
  val word = P.charsWhile1(('a' to 'z').contains)

  val nat = (Numbers.nonZeroDigit ~ Numbers.digits).map((s, r) => s"$s$r".toInt)

  val one = P.char('1')

  val adjColor = (word <* sp) ~ (word <* sp)

  val noneRule = P.string1("no other bags").as(List.empty[SubRule])
  val oneRule = ((one <* sp) *> adjColor <* P.string1("bag")).map { case (a, c) => SubRule(1, Bag(a, c)) }
  val multiRule = ((nat <* sp) ~ adjColor <* P.string1("bags")).map { case (n, (a, c)) => SubRule(n, Bag(a, c)) }

  val natRule = P.oneOf1(oneRule :: multiRule :: Nil)

  val natRules = P.repSep(natRule, min = 1, sep = P.string1(", "))

  val subRules = P.oneOf(noneRule :: natRules :: Nil)

  val bag = (adjColor <* P.string1("bags contain ")).map(Bag(_,_))

  (bag ~ subRules <* (P.char('.') <* P.end)).map(Rule(_,_))

def parse(line: String) = rule.parse(line).map((_, rule) => rule)

def _n[T](f: Seq[Rule] => T) =
  for
    lines <- io.unsafe.lines(challenge(day=7, part=0))
    rules <- traverse(lines)(parse)
  yield
    f(rules)

val _0 = _n(_.filter(_.rules.isEmpty)).eval
