import challenges._

import collectionutil._

import Code._

type Passport = Map[Code, String]
type Policy   = Passport => Boolean
type Rule     = String => Boolean

enum Code:
  case Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid, Cid

val Codes = Code.values.map(c => c.toString.toLowerCase -> c).toMap

def process(lines: Seq[String]): Either[String, Seq[Passport]] =

  val Field = s"(${Codes.keys.mkString("|")}):(.+)".r

  def fields(lines: Seq[String]) =
    val noSpaces = lines.map(_.split(" ").filter(_.trim.nonEmpty)).flatten
    traverse(noSpaces) {
      case Field(code, value) => Right(Codes(code) -> value)
      case unknown            => Left(s"unknown field $unknown")
    }

  traverse(groupLines(lines))(fields).map(_.map(_.toMap))

end process

def Year(in: Range) = rule(raw"(\d\d\d\d)".r) {
  case Seq(i) => in.contains(i.toInt)
}

def Height(cm: Range, in: Range) = rule(raw"(\d+)((?:cm|in))".r) {
  case Seq(i, "cm") => cm.contains(i.toInt)
  case Seq(i, "in") => in.contains(i.toInt)
}

val Color = regex(raw"#(?:[0-9]|[a-f]){6}".r)
val Eye = regex(raw"amb|blu|brn|gry|grn|hzl|oth".r)
val Pin = regex(raw"\d{9}".r)

def regex(r: util.matching.Regex): Rule =
  rule(r)(Function.const(true))

def rule(r: util.matching.Regex)(process: Seq[String] => Boolean): Rule =
  r.unapplySeq(_).map(process).fold(false)(identity)

val Rules: Map[Code, Rule] = Map(
  Byr -> Year(1920 to 2002),
  Iyr -> Year(2010 to 2020),
  Eyr -> Year(2020 to 2030),
  Hgt -> Height(cm = 150 to 193, in = 59 to 76),
  Hcl -> Color,
  Ecl -> Eye,
  Pid -> Pin,
  Cid -> Function.const(true),
)

val Required = Code.values.toSet - Code.Cid

val AllFields: Policy = allFields(Required)
val AllValid: Policy = allValid(Required, Rules)

def allFields(required: Set[Code]): Policy =
  passport => (required -- passport.keys).isEmpty

def allValid(required: Set[Code], validation: Code => Rule): Policy =
  allFields(required) && (_.forall(validation(_)(_)))

def _n(policy: Policy) =
  for
    lines     <- io.unsafe.lines(challenge(day=4, part=0))
    passports <- process(lines)
  yield
    passports.map(policy).count(identity)

val _0 = _n(AllFields).eval
val _1 = _n(AllValid).eval
