package net.shift
package security

import scala.util.Failure
import scala.util.Try
import scala.util.matching.Regex
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import net.shift.common.ShiftFailure
import net.shift.common.Implicits

trait Credentials

sealed case class BasicCredentials(userName: String, password: String) extends Credentials

case class SecurityFailure[+T](msg: String) extends RuntimeException(msg) with util.control.NoStackTrace {
  def toFailure = Failure(this)
}

object Permissions {
  def unapplySeq(s: String): Option[Seq[Permission]] = Some(s.split(",").toList.map(Permission(_)))
}

case class Permission(name: String)

case class Organization(name: String)

object Users {
  def unapply(s: String): Option[User] = {
    s.split(":").toList match {
      case user :: Nil =>
        Some(User(user, None, Set.empty))
      case user :: "" :: Permissions(perms @ _*) :: Nil =>
        Some(User(user, None, perms.toSet))
      case user :: org :: Nil =>
        Some(User(user, Some(Organization(org)), Set.empty))
      case user :: org :: Permissions(perms @ _*) :: Nil =>
        Some(User(user, Some(Organization(org)), perms.toSet))
    }
  }
}

case class User(name: String, org: Option[Organization], permissions: Set[Permission]) {
  def requireAll[T](perms: Permission*)(f: => T): Try[T] = {
    if (perms.toSet diff permissions isEmpty) {
      Try(f)
    } else {
      ShiftFailure[T]("not.enough.permissions").toFailure
    }
  }

  def requireSome[T](perms: Permission*)(f: => T): Try[T] = {
    val diff = perms.toSet diff permissions

    if (diff.size < perms.size) {
      Try(f)
    } else {
      ShiftFailure[T]("not.enough.permissions").toFailure
    }
  }

}

object HMac {

  def encodeSHA256(message: String, salt: String): Array[Byte] = {
    val hmac = Mac.getInstance("HmacSHA256");
    val secretKey = new SecretKeySpec(salt.getBytes(), "HmacSHA256");
    hmac.init(secretKey);
    hmac.doFinal()
  }

}

object Sec extends App {

  val u1 = User("me", None, Set(Permission("read"), Permission("write"), Permission("delete")))
  val res1 = u1.requireAll(Permission("read")) {
    println("do something 1")
    1
  }

  val res2 = u1.requireAll(Permission("read"), Permission("create")) {
    println("do something 2")
    1
  }

  val res3 = u1.requireSome(Permission("read"), Permission("create")) {
    println("do something 3")
    1
  }

  println(res1)
  println(res2)
  println(res3)

  val Pattern = new Regex("""(\w+):(\w*):([\w,]+)""")

  val parse = "marius::read,write" match {
    case Pattern(user, "", perms)  => Some(User(user, None, perms.split(",").map(Permission(_)).toSet))
    case Pattern(user, org, perms) => Some(User(user, Some(Organization(org)), perms.split(",").map(Permission(_)).toSet))
    case _                         => None
  }

  println(parse)
}
