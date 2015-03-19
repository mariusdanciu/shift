package net.shift
package html

import scala.xml._

import common._

sealed trait Validation[+E, +A] {
  def map[B](f: A => B): Validation[E, B] = this match {
    case f @ Invalid(e) => f
    case _              => Valid(f(get))
  }

  def flatMap[AA >: A, EE >: E, B](f: A => Validation[EE, B]): Validation[EE, B] = this match {
    case f @ Invalid(e) => f
    case _              => f(get)
  }

  def isError: Boolean
  protected def get: A
}

case class Invalid[E](e: E) extends Validation[E, Nothing] {
  val isError = true
  protected def get = throw new UnsupportedOperationException
}

case class Valid[A](a: A) extends Validation[Nothing, A] {
  val isError = false
  protected def get = a
}

case class FormletErr(name: String, message: String)

class ReversedApplicativeForm[A, Env, Err](form: Formlet[A, Env, Err]) {

  def <*>[X, Y](f: Formlet[X, Env, Err])(implicit e: A <:< (X => Y), s: Semigroup[Err]): Formlet[Y, Env, Err] = new Formlet[Y, Env, Err] {

    override val validate: Env => Validation[Err, Y] = env => {
      (form.validate(env), f.validate(env)) match {
        case (Invalid(e1), Invalid(e2)) => Invalid(s append (e1, e2))
        case (_, Invalid(e))            => Invalid(e)
        case (Invalid(e), _)            => Invalid(e)
        case (Valid(a1), Valid(a2))     => Valid(a1(a2))
      }
    }
    override def html = form.html ++ f.html
  }

}

trait Formlet[A, Env, Err] { me =>
  type ValidationRule = Env => Validation[Err, A]

  def validate: ValidationRule

  def html: NodeSeq = NodeSeq.Empty

  def label(id: String, text: String): Formlet[A, Env, Err] = new Formlet[A, Env, Err] {
    val validate = me validate
    override def html: NodeSeq = <label for={ id }>{ text }</label> ++ me.html
  }

  def attr(name: String, value: String): Formlet[A, Env, Err] = new Formlet[A, Env, Err] {
    def validate = me validate
    override def html: NodeSeq = me.html match {
      case elem: Elem => elem % new UnprefixedAttribute(name, value, Null)
      case e          => e
    }
  }

  def decorate(f: NodeSeq => NodeSeq) = new Formlet[A, Env, Err] {
    def validate = me validate
    override def html: NodeSeq = f(me.html)
  }

}

object Formlet {

  implicit def formToApp[A, Env, Err](form: Formlet[A, Env, Err]): ReversedApplicativeForm[A, Env, Err] = new ReversedApplicativeForm(form)

  implicit def listSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    def append(a: List[A], b: List[A]): List[A] = a ::: b
  }

  def apply[A, Env, Err](a: => A): Formlet[A, Env, Err] = new Formlet[A, Env, Err] {
    val validate: Env => Validation[Err, A] = env => Valid(a)
    override def html = NodeSeq.Empty
  }

  def inputText[Env, Err, S](name: String)(f: Env => Validation[Err, S]) = new Formlet[S, Env, Err] {
    val validate = f
    override def html = <input type="text" name={ name }/>
  }

  def inputDouble[Env, Err](name: String)(f: Env => Validation[Err, Double]) = new Formlet[Double, Env, Err] {
    val validate = f
    override def html = <input type="text" name={ name }/>
  }

  def inputOptional[Env, Err, T](name: String)(f: Env => Validation[Err, Option[T]]) = new Formlet[Option[T], Env, Err] {
    val validate = f
    override def html = <input type="text" name={ name }/>
  }

  def inputInt[Env, Err](name: String)(f: Env => Validation[Err, Int]) = new Formlet[Int, Env, Err] {
    val validate = f
    override def html = <input type="text" name={ name }/>
  }

  def inputCheck[Env, Err](name: String, value: String)(f: Env => Validation[Err, Boolean]) = new Formlet[Boolean, Env, Err] {
    val validate = f
    override def html = <input type="checkbox" name={ name } value={ value }/>
  }

  def inputRadio[Env, Err](name: String, value: String)(f: Env => Validation[Err, Boolean]) = new Formlet[Boolean, Env, Err] {
    val validate = f
    override def html = <input type="checkbox" name={ name } value={ value }/>
  }

  def inputHidden[Env, Err, S](name: String, value: String)(f: Env => Validation[Err, S]) = new Formlet[S, Env, Err] {
    val validate = f
    override def html = <input type="hidden" name={ name } value={ value }/>
  }

  def inputPassword[Env, Err](name: String)(f: Env => Validation[Err, String]) = new Formlet[String, Env, Err] {
    val validate = f
    override def html = <input type="password" name={ name }/>
  }

  def inputSelect[Env, Err, S](name: String, options: List[(String, String)])(f: Env => Validation[Err, S]) = new Formlet[S, Env, Err] {
    val validate = f
    override def html = <select name={ name }>{ options.map { o => <option value={ o._1 }>{ o._2 }</option> } }</select>
  }

  def inputFile[Env, Err, S](name: String)(f: Env => Validation[Err, S]) = new Formlet[S, Env, Err] {
    val validate = f
    override def html = <input type="file" name={ name }/>
  }

}





