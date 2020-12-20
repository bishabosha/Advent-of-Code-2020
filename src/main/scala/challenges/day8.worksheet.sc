import challenges._

import collectionutil._

type Instruction = Machine => Machine
type Operation   = Int => Instruction

case class Machine(acc: Int, pointer: Int, seen: Set[Int])

def oneStep(accumulate: Int, advance: Int): Instruction = machine =>
  Machine(
    acc = machine.acc + accumulate,
    pointer = machine.pointer + advance,
    seen = machine.seen + machine.pointer)

val Code: Map[String, Operation] = Map(
  "acc" -> (a => oneStep(accumulate = a, advance = 1)),
  "jmp" -> (a => oneStep(accumulate = 0, advance = a)),
  "nop" -> (a => oneStep(accumulate = 0, advance = 1))
)

val Instruction = raw"(${Code.keys.mkString("|")}) ([+-])(\d+)".r

def readInstruction(str: String): Either[String, Instruction] = str match
  case Instruction(code, sign, arg) =>
    def value(i: Int) = if sign == "-" then -i else i
    Right(Code(code)(value(arg.toInt)))
  case _ => Left(s"not an instruction, got $str")

def runMachine(instructions: Seq[Instruction]): Machine =
  val ops = instructions.toArray
  def haltAtLoop(curr: Machine): Machine =
    if curr.seen.contains(curr.pointer) then curr
    else haltAtLoop(ops(curr.pointer)(curr))
  haltAtLoop(Machine(acc = 0, pointer = 0, seen = Set()))

def accAtHalt = runMachine andThen (_.acc)

def _n[T](op: Seq[Instruction] => T) =
  for
    lines <- io.unsafe.lines(challenge(day=8, part=0))
    instructions <- traverse(lines)(readInstruction)
  yield
    op(instructions)

val _1 = _n(accAtHalt).eval
