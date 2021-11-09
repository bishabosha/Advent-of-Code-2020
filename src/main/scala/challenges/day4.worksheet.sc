import challenges.*

import collectionutil.*

import Code.*
import Rule.*

type Passport = Map[Code, String]
type Policy = Passport => Boolean
type Validation = String => Boolean
type Lookup = Code => Rule

enum Code:
  case Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid, Cid

object Validation:

  val anything = Function.const(true)

  def regex(r: util.matching.Regex): Validation =
    rule(r)(anything)

  def rule(r: util.matching.Regex)(process: Seq[String] => Boolean): Validation =
    r.unapplySeq(_).map(process).fold(false)(identity)

def process(lines: Seq[String]): Either[String, Seq[Passport]] =

  val codes = Map(Code.values.map(c => c.toString.toLowerCase -> c)*)
  val Field = rx"(${codes.keys.mkString("|")}):(.+)"

  def fields(lines: Seq[String]) =
    val noSpaces = lines.map(_.split(" ").filter(_.trim.nonEmpty)).flatten
    traverse(noSpaces) {
      case Field(code, value) => Right(codes(code) -> value)
      case unknown            => Left(s"unknown field $unknown")
    }

  traverse(groupLines(lines))(fields).map(_.map(_.toMap))

end process

enum Rule:
  case Year(in: Range)
  case Height(cm: Range, in: Range)
  case Color, Eye, Pin, All

  lazy val validate: Validation = this match
    case Year(in: Range) =>
      Validation.rule(rx"(\d\d\d\d)") { case Seq(i) =>
        in.contains(i.toInt)
      }
    case Height(cm: Range, in: Range) =>
      Validation.rule(rx"(\d+)((?:cm|in))") {
        case Seq(i, "cm") => cm.contains(i.toInt)
        case Seq(i, "in") => in.contains(i.toInt)
      }
    case Color => Validation.regex(rx"#(?:[0-9]|[a-f]){6}")
    case Eye   => Validation.regex(rx"amb|blu|brn|gry|grn|hzl|oth")
    case Pin   => Validation.regex(rx"\d{9}")
    case All   => Validation.anything

end Rule

lazy val Rules: Lookup =
  case Byr => Year(1920 to 2002)
  case Iyr => Year(2010 to 2020)
  case Eyr => Year(2020 to 2030)
  case Hgt => Height(cm = 150 to 193, in = 59 to 76)
  case Hcl => Color
  case Ecl => Eye
  case Pid => Pin
  case Cid => All

val Required = Code.values.toSet - Code.Cid

lazy val AllFields = allFields(Required)
lazy val AllValid = allValid(Required, Rules)

def allFields(required: Set[Code]): Policy =
  passport => (required -- passport.keys).isEmpty

def allValid(required: Set[Code], rule: Lookup): Policy =
  allFields(required) && (_.forall(rule(_).validate(_)))

def run(policy: Policy)(passports: Seq[Passport]): Int =
  passports.map(policy).count(identity)

lazy val _n =
  for
    lines <- io.unsafe.lines(challenge(day = 4, part = 0))
    passports <- process(lines)
  yield passports

val _0 = _n.map(run(AllFields)).eval
val _1 = _n.map(run(AllValid)).eval
