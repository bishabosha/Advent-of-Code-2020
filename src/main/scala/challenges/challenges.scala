package challenges

def challenge(day: Int, part: Int)(pwd: os.Path) = pwd / "inputs" / s"day$day-$part"

extension [E, A](e: Either[E, A]) def eval = e.fold(e => s"error [${e.getClass.getCanonicalName}]: $e", identity)

extension (sc: StringContext) inline def rx(parts: String*) = sc.raw(parts*).r
