package net.shift
package html

import common._
import scala.xml._

sealed trait Validation[+E, +A] 
case class Failure[E](e: E) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

case class FormletErr(name: String, message: String)

class ReversedApplicativeForm[A, Env, Err](form: Form[A, Env, Err]) {

  def <*>[X, Y](f: Form[X, Env, Err])(implicit e: A <:< (X => Y), s: Semigroup[Err]): Form[Y, Env, Err] = new Form[Y, Env, Err] {
    override val validate : Env => Validation[Err, Y] = env => {
	  (form.validate(env), f.validate(env)) match {
	    case (Failure(e1), Failure(e2)) => Failure(s append (e1, e2))
		case (_, Failure(e)) => Failure(e)
		case (Failure(e), _) => Failure(e)
		case (Success(a1), Success(a2)) => Success( a1( a2 ) )
	  }
	}
	override def html = form.html ++ f.html
  }

}

trait Form[A, Env, Err] {me =>
  val validate : Env => Validation[Err, A]

  def html: NodeSeq = NodeSeq.Empty
  
  def label(id: String, text: String): Form[A, Env, Err] = new Form[A, Env, Err] {
	val validate = me validate
	override def html: NodeSeq = <label for={id}>{text}</label> ++ me.html
  }

  def attr(name: String, value: String): Form[A, Env, Err] = new Form[A, Env, Err] {
	val validate = me validate
	override def html: NodeSeq = me.html match {
	  case elem : Elem => elem % new UnprefixedAttribute(name, value, Null)
	  case e => e
	}
  }
  
}

trait Semigroup[A] {
  def append(a: A, b: A): A
}

object Formlet {

  implicit def formToApp[A, Env, Err](form: Form[A, Env, Err]): ReversedApplicativeForm[A, Env, Err] = new ReversedApplicativeForm(form)

  implicit def listSemigroup[A]: Semigroup[List[A]] =  new Semigroup[List[A]] {
    def append(a: List[A], b: List[A]): List[A] = a ::: b
  }

  def apply [A, Env, Err](a: => A): Form[A, Env, Err] = new Form[A, Env, Err] {
	val validate : Env => Validation[Err, A] = env => Success(a)
	override def html = NodeSeq.Empty
  }
  
  def inputText[Env, Err](name: String)(f: Env => Validation[Err, String]) = new Form[String, Env, Err] {
    val validate = f
	override def html = <input type="text" name={name} />
  }
  
  def inputInt[Env, Err](name: String)(f: Env => Validation[Err, Int]) = new Form[Int, Env, Err] {
    val validate = f
	override def html = <input type="text" name={name} />
  }
  
}


