import net.shift.http.Path

package object shiftdefs {

  val / = Path

  type ?=>[-A, +B] = PartialFunction[A, B]

  implicit def str2Path(part: String) = Path(part)
}

