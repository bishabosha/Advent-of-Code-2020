package challenges

def challenge(day: Int, part: Int)(pwd: os.Path) = pwd / "inputs" / s"day$day-$part"

extension [E, A](e: Either[E, A]) def eval = e.fold(e => s"error: $e", identity)
