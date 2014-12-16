package net.shift
package html

import scala.xml._

import common._

sealed trait Validation[+E, +A] {
  def map[B](f: A => B): Validation[E, B] = this match {
    case f @ Failure(e) => f
    case _              => Success(f(get))
  }

  def flatMap[AA >: A, EE >: E, B](f: AA => Validation[EE, B]): Validation[EE, B] = this match {
    case f @ Failure(e) => f
    case _              => f(get)
  }

  def postValidate[EE >: E, AA >: A](e: A => Validation[EE, AA]): Validation[EE, AA] = e(get)

  def isError: Boolean
  protected def get: A
}

case class Failure[E](e: E) extends Validation[E, Nothing] {
  val isError = true
  protected def get = throw new UnsupportedOperationException
}

case class Success[A](a: A) extends Validation[Nothing, A] {
  val isError = false
  protected def get = a
}

case class FormletErr(name: String, message: String)

class ReversedApplicativeForm[A, Env, Err](form: Formlet[A, Env, Err]) {

  def <*>[X, Y](f: Formlet[X, Env, Err])(implicit e: A <:< (X => Y), s: Semigroup[Err]): Formlet[Y, Env, Err] = new Formlet[Y, Env, Err] {

    override val validate: Env => Validation[Err, Y] = env => {
      (form.validate(env), f.validate(env)) match {
        case (Failure(e1), Failure(e2)) => Failure(s append (e1, e2))
        case (_, Failure(e))            => Failure(e)
        case (Failure(e), _)            => Failure(e)
        case (Success(a1), Success(a2)) => Success(a1(a2))
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
    val validate: Env => Validation[Err, A] = env => Success(a)
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
    override def html = <select name={ name }>{ options.map { o => <option name={ o._1 }>{ o._2 }</option> } }</select>
  }

  def inputFile[Env, Err, S](name: String)(f: Env => Validation[Err, S]) = new Formlet[S, Env, Err] {
    val validate = f
    override def html = <input type="file" name={ name }/>
  }

}

object Main extends App with XmlUtils {

  case class Person(name: String, age: Int)
  case class Subject(person: Person, userName: String)

  val person = (Person(_, _)).curried
  val subject = (Subject(_, _)).curried

  import Formlet._

  def validName(name: String): Map[String, String] => Validation[List[String], String] = env => env.get(name) match {
    case Some(n) => Success(n);
    case _       => Failure(List("Missing name value"))
  }

  def validAge: Map[String, String] => Validation[List[String], Int] = env => env.get("age") match {
    case Some(age) => try {
      val intAge = age.toInt
      if (intAge >= 18)
        Success(intAge)
      else
        Failure(List("Age must be higher than 18"))
    } catch {
      case e: Exception => Failure(List(age + " is not a number"))
    }
    case _ => Failure(List("Missing name value"))
  }

  val form = Formlet(person) <*>
    inputText("name")(validName("name")).label("id", "User name: ") <*>
    inputInt("age")(validAge).attr("id", "ageId")

  println(form html)

  val p = (form validate Map(("name" -> "marius"), ("age" -> "33"))) postValidate {
    case p @ Person("marius", age) => Failure(List("Unacceptable person"))
    case p                         => Success(p)
  }

  println(p)

  // Now let's compose forms

  val subjectForm = Formlet(subject) <*> form <*> inputText("address")(validName("userName"))
  val markup = subjectForm.html
  println(markup)
  println(elemByAttr(markup, ("id", "ageId")))
  println(elemByAttr(markup, ("name", "address")))

  println(subjectForm validate Map(("name" -> "marius"), ("userName" -> "mda"), ("age" -> "33")))

}



