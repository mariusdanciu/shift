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

case class SecurityFailure[+T](msg: String, code: Int = 401) extends RuntimeException(msg) with util.control.NoStackTrace {
  def toTry = Failure(this)
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

  def hasAllPermissions(perms: Permission*) = perms.toSet diff permissions isEmpty

  def requireAll[T](perms: Permission*)(f: => T): Try[T] = {
    if (perms.toSet diff permissions isEmpty) {
      Try(f)
    } else {
      ShiftFailure("not.enough.permissions").toTry
    }
  }

  def notThesePermissions[T](perms: Permission*)(f: => T): Try[T] = {
    val l = perms.size
    if (perms.toSet.diff(permissions).size == l) {
      Try(f)
    } else {
      ShiftFailure("not.enough.permissions").toTry
    }
  }

  def requireSome[T](perms: Permission*)(f: => T): Try[T] = {
    val diff = perms.toSet diff permissions

    if (diff.size < perms.size) {
      Try(f)
    } else {
      ShiftFailure("not.enough.permissions").toTry
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


