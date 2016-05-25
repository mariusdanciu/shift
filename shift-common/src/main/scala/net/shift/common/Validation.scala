package net.shift.common

object Validator {

  def apply[A, Env, Err](f: Env => Validation[A, Err]) = new Validator[A, Env, Err] {
    def validate = f
  }

  def apply[A, Env, Err](v: => A) = new Validator[A, Env, Err] {
    def validate = env => Valid(v)
  }
}

trait Validator[A, Env, Err] { me =>
  type ValidationRule = Env => Validation[A, Err]

  def validate: ValidationRule

  def <*>[X, Y](v: Validator[X, Env, Err])(implicit evidence: A <:< (X => Y)): Validator[Y, Env, Err] = new Validator[Y, Env, Err] {
    def validate: Env => Validation[Y, Err] = env =>
      (me.validate(env), v.validate(env)) match {
        case (Invalid(err1), Invalid(err2)) => Invalid(err1 ++ err2)
        case (_, Invalid(err2))             => Invalid(err2)
        case (Invalid(err1), _)             => Invalid(err1)
        case (Valid(a), Valid(b))           => Valid(a(b))
      }
  }

}

case class Form(name: String, age: Int, balance: Double)

sealed trait Validation[+T, E] {
  
  def flatMap[B](f: T => Validation[B, E]): Validation[B, E]
}

case class Valid[T, E](value: T) extends Validation[T, E] {
  def flatMap[B](f: T => Validation[B, E]): Validation[B, E] = f(value)
}
case class Invalid[E](errors: List[E]) extends Validation[Nothing, E] {
  def flatMap[B](f: Nothing => Validation[B, E]): Validation[B, E] = this
}

