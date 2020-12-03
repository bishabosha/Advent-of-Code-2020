package challenges

def challenge(day: Int, part: Int)(pwd: os.Path) = pwd / "inputs" / s"day$day-$part"

extension [E, A](e: Either[E, A]) def eval = println(e.fold(e => s"error: $e", v => s"result: $v"))
