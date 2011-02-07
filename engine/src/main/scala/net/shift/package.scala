import net.shift.http.Path

package object shiftdefs {


  type ?=>[-A, +B] = PartialFunction[A, B]
  
  val / = Path
  implicit def str2Path(part: String) = Path(part)
}

