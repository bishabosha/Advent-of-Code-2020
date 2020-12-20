import challenges._

import collectionutil._

type Instruction = Machine => Machine
type Operation   = Int => Instruction

case class Op(code: String, args: Int)
case class Machine(acc: Int, pointer: Int)

def oneStep(accumulate: Int, advance: Int, code: String, arg: Int): Instruction = machine =>
  Machine(acc = machine.acc + accumulate, pointer = machine.pointer + advance)

val Code: Map[String, Operation] = Map(
  "acc" -> (a => oneStep(accumulate = a, advance = 1, code = "acc", arg = a)),
  "jmp" -> (a => oneStep(accumulate = 0, advance = a, code = "jmp", arg = a)),
  "nop" -> (a => oneStep(accumulate = 0, advance = 1, code = "nop", arg = a))
)

val Instruction = raw"(${Code.keys.mkString("|")}) ([+-])(\d+)".r

def readInstruction(str: String): Either[String, (Op, Instruction)] = str match
  case Instruction(code, sign, arg) =>
    def value(i: Int) = if sign == "-" then -i else i
    val i = value(arg.toInt)
    Right(Op(code, i) -> Code(code)(i))
  case _ => Left(s"not an instruction, got $str")

def runMachine(instructions: Seq[(Op, Instruction)]): Machine =
  val ops = instructions.map(_(1)).toArray
  def halt(curr: Machine, seen: Set[Int]): Machine =
    if seen.contains(curr.pointer) then curr
    else
      halt(ops(curr.pointer)(curr), seen + curr.pointer)
  halt(Machine(acc = 0, pointer = 0), Set())

def subs(instructions: Seq[(Op, Instruction)]): Seq[(Int, Instruction)] =
  instructions.zipWithIndex.map {
    case ((Op("nop", a), inst), i) if a != 0 => Some((i, Code("jmp")(a)))
    case ((Op("jmp", a), inst), i)           => Some((i, Code("nop")(a)))
    case _                                   => None
  }.flatten

def runMachinePatches(instructions: Seq[(Op, Instruction)]): Either[String, Machine] =
  val patches = subs(instructions)
  val ops0 = instructions.map(_(1)).toArray
  val opss = LazyList(patches:_*).map(ops0.updated(_, _))
  def runMachineWith(ops: Array[Instruction]): Option[Machine] =
    def halt(curr: Machine, seen: Set[Int]): Option[Machine] =
      if seen.contains(curr.pointer) then None
      else if curr.pointer == ops.length  then Some(curr)
      else
        halt(ops(curr.pointer)(curr), seen + curr.pointer)
    halt(Machine(acc = 0, pointer = 0), Set())
  opss.map(runMachineWith).collectFirst {
    case Some(curr) => curr
  }.toRight("no patch could terminate the program")

def accAtLoop = runMachine andThen (_.acc)
def accAtHalt = runMachinePatches andThen (_.map(_.acc))

def _n[T](op: Seq[(Op, Instruction)] => T) =
  for
    lines <- io.unsafe.lines(challenge(day=8, part=0))
    instructions <- traverse(lines)(readInstruction)
  yield
    op(instructions)

val _1 = _n(accAtLoop).eval
val _2 = _n(accAtHalt).flatten.eval
