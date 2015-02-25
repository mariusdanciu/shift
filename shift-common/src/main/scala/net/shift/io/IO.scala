package net.shift
package io

import java.io.FileInputStream
import java.io.InputStream
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import net.shift.common.Path
import java.io.BufferedInputStream
import scala.xml.NodeSeq
import net.shift.common.XmlUtils._
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import java.io.OutputStream
import java.io.Closeable

object IO extends App {

  type BinConsumer[O] = Iteratee[Array[Byte], O]
  type Producer[I] = IterateeProducer[I]
  type BinProducer = IterateeProducer[Array[Byte]]

  def close(c: Closeable) = try {
    c.close
  } catch {
    case e: Exception =>
  }

  def fileProducer(p: Path, bufSize: Int = 32768): Try[BinProducer] = try {
    Success(inputStreamProducer(new BufferedInputStream(new FileInputStream(p.toString()), bufSize), bufSize))
  } catch {
    case e: Exception => Failure(e)
  }

  private def singleProducer[O](in: In[Array[Byte]]) = new BinProducer {

    def apply[O](it: Iteratee[Array[Byte], O]): Iteratee[Array[Byte], O] = it match {
      case Cont(f) => f(in) match {
        case Cont(g) => g(EOF)
        case r       => r
      }
      case r => r
    }
  }

  def emptyProducer = new BinProducer {

    def apply[O](it: Iteratee[Array[Byte], O]): Iteratee[Array[Byte], O] = it match {
      case Cont(f) => f(Empty)
      case state   => state
    }
  }

  def stringProducer(s: String): BinProducer = singleProducer(Data(s.getBytes("UTF-8")))

  def arrayProducer(arr: Array[Byte]): BinProducer = singleProducer(Data(arr))

  def htmlProducer(s: NodeSeq): Try[BinProducer] = Try(singleProducer(Data(("<!DOCTYPE html>\n" + mkString(s)).getBytes("UTF-8"))))

  def inputStreamProducer(in: InputStream, bufSize: Int = 32768) = new BinProducer {

    @tailrec
    def apply[O](it: Iteratee[Array[Byte], O]): Iteratee[Array[Byte], O] = {
      val buf = Array.ofDim[Byte](bufSize)

      it match {
        case Cont(f) =>
          var r = in.read(buf);
          if (r > -1)
            apply(f(Data(buf.take(r))))
          else {
            close(in)
            apply(f(EOF))
          }
        case e @ Error(t) =>
          close(in)
          e
        case done @ Done(v, rest) =>
          done
      }
    }
  }

  def toArray(in: BinProducer): Try[Array[Byte]] = {
    in(Iteratee.foldLeft(new ArrayBuffer[Byte]) { (acc, e) =>
      acc ++ e
    }) match {
      case Done(v, _) => Success(v.toArray)
      case Error(t)   => Failure(t)
      case _          => Failure(new IllegalStateException)
    }
  }

  def toArray(in: InputStream, bufSize: Int = 32768): Try[Array[Byte]] = {
    inputStreamProducer(in)(Iteratee.foldLeft(new ArrayBuffer[Byte](bufSize)) { (acc, e) =>
      acc ++ e
    }) match {
      case Done(v, _) => Success(v.toArray)
      case Error(t)   => Failure(t)
      case _          => Failure(new IllegalStateException)
    }
  }

  def pathWriter(path: Path): Try[Iteratee[Array[Byte], Path]] =
    Try {
      for {
        out <- Iteratee.foldLeft[Array[Byte], OutputStream](new BufferedOutputStream(new FileOutputStream(path.toString))) { (acc, e) => { acc.write(e); acc } }
      } yield {
        IO close out
        path
      }
    }

}

sealed trait Iteratee[I, O] { self =>
  def map[B](f: O => B): Iteratee[I, B]
  def flatMap[B](f: O => Iteratee[I, B]): Iteratee[I, B]
  def filter(f: O => Boolean): Iteratee[I, O]
}

case class Cont[I, O](g: In[I] => Iteratee[I, O]) extends Iteratee[I, O] {
  def map[B](f: O => B): Iteratee[I, B] = Cont(i => g(i) map f)
  def flatMap[B](f: O => Iteratee[I, B]) = Cont(i => g(i) flatMap f)
  def filter(f: O => Boolean) = Cont(i => g(i) filter f)
}

case class Done[I, O](v: O, rest: In[I]) extends Iteratee[I, O] {
  def map[B](f: O => B): Iteratee[I, B] = Done(f(v), rest)
  def flatMap[B](f: O => Iteratee[I, B]) = f(v) match {
    case Done(d, r)     => Done(d, rest)
    case Cont(g)        => g(rest)
    case err @ Error(_) => err
  }

  def filter(f: O => Boolean) = if (f(v))
    Done(v, rest)
  else
    Error(new RuntimeException("Failed filter predicate"))
}

case class Error[I, O](t: Throwable) extends Iteratee[I, O] {
  def map[B](f: O => B): Iteratee[I, B] = Error(t)
  def flatMap[B](f: O => Iteratee[I, B]) = Error(t)
  def filter(f: O => Boolean) = Error(t)
}

object In {
  def apply[T](v: T): In[T] = Data(v)
  def fail(t: Throwable) = Fail(t)
}

trait In[+T]
case class Data[+T](v: T) extends In[T]
case object EOF extends In[Nothing]
case object Empty extends In[Nothing]
case class Fail(t: Throwable) extends In[Nothing]

trait IterateeProducer[I] {
  def apply[O](it: Iteratee[I, O]): Iteratee[I, O]
}

object Iteratee {

  def foldLeft[I, O](initial: O)(f: (O, I) => O): Iteratee[I, O] = {
    def step(o: O)(i: In[I]): Iteratee[I, O] = {
      i match {
        case Data(v) =>
          try {
            Cont(step(f(o, v)))
          } catch {
            case e: Exception => Error(e)
          }
        case EOF     => Done(o, i)
        case Fail(t) => Error(t)
      }
    }
    Cont(step(initial))
  }

}

object IterateeProducer {

  def traverse[T](t: Traversable[In[T]]) = new IterateeProducer[T] {
    def apply[O](it: Iteratee[T, O]): Iteratee[T, O] = (it /: t) { (acc, el) =>
      acc match {
        case Cont(f) => f(el)
        case state   => state
      }
    }
  }

}