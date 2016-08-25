package net.shift
package io

import java.io.Closeable
import java.io.FileInputStream
import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.Exception.catching
import scala.xml.NodeSeq
import net.shift.common.Path
import net.shift.common.XmlUtils.mkString
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption

object IODefaults {

  implicit val fs: FileSystem = LocalFileSystem
}

object IO extends App {

  def close(c: Closeable) = try {
    c.close
  } catch {
    case e: Exception =>
  }

  def failover[I, O](f: => Iteratee[I, O]): Iteratee[I, O] =
    catching(classOf[Exception]).withApply[Iteratee[I, O]](e => Error(e)) {
      f
    }

  def fromArray[O](in: ByteBuffer): BinProducer = fromChunks(List(in))

  def fromChunks[O](in: Seq[ByteBuffer]): BinProducer = {
    val data = (in map { d => Data(d) }) ++ List(EOF)
    new BinProducer {

      def apply[O](it: Iteratee[ByteBuffer, O]): Iteratee[ByteBuffer, O] = {
        (it /: data) {
          case (Cont(f), e) => f(e)
          case (r, _)       => r
        }
      }
    }
  }

  def append[O](b: BinProducer, in: ByteBuffer): BinProducer = {
    new BinProducer {

      def apply[O](it: Iteratee[ByteBuffer, O]): Iteratee[ByteBuffer, O] = {
        val res = b(it)
        singleProducer(Data(in))(res)
      }
    }
  }

  def singleProducer[O](in: In[ByteBuffer]) = new BinProducer {

    def apply[O](it: Iteratee[ByteBuffer, O]): Iteratee[ByteBuffer, O] = it match {
      case Cont(f) => f(in) match {
        case Cont(g) => g(EOF)
        case r       => r
      }
      case r => r
    }
  }

  def emptyProducer = new BinProducer {

    def apply[O](it: Iteratee[ByteBuffer, O]): Iteratee[ByteBuffer, O] = it match {
      case Cont(f) => f(Empty)
      case state   => state
    }
  }

  def stringProducer(s: String): BinProducer = singleProducer(Data(ByteBuffer.wrap(s.getBytes("UTF-8"))))

  def arrayProducer(arr: Array[Byte]): BinProducer = singleProducer(Data(ByteBuffer.wrap(arr)))

  def bufferProducer(buffer: ByteBuffer): BinProducer = singleProducer(Data(buffer))

  def htmlProducer(s: NodeSeq): Try[BinProducer] = Try(stringProducer("<!DOCTYPE html>\n" + mkString(s)))

  def fileProducer(path: Path, bufSize: Int = 32768): Try[BinProducer] = Try {

    new BinProducer {

      def apply[O](ait: Iteratee[ByteBuffer, O]): Iteratee[ByteBuffer, O] = {

        @tailrec
        def loop(it: Iteratee[ByteBuffer, O], fc: FileChannel): Iteratee[ByteBuffer, O] = {
          val b = ByteBuffer.allocate(bufSize)
          val read = fc.read(b)

          if (read != -1) {
            b.flip
            val chunk = ByteBuffer.allocate(read)
            chunk.put(b)
            chunk.flip

            val s = IO.bufferToString(chunk)

            it match {
              case Cont(f) => loop(f(Data(chunk)), fc)
              case r       => r
            }
          } else {
            it match {
              case Cont(f) => f(EOF)
              case r       => r
            }
          }
        }

        val fc = new FileInputStream(path.toString).getChannel
        val it = failover(loop(ait, fc))
        fc.close()
        it
      }

    }
  }

  def inputStreamProducer(in: InputStream, bufSize: Int = 32768) = new BinProducer {

    def apply[O](ait: Iteratee[ByteBuffer, O]): Iteratee[ByteBuffer, O] = {

      @tailrec
      def walk[O](it: Iteratee[ByteBuffer, O]): Iteratee[ByteBuffer, O] = {

        val buf = Array.ofDim[Byte](bufSize)

        it match {
          case Cont(f) =>
            var r = in.read(buf);
            val bf = ByteBuffer.wrap(buf, 0, r)

            if (r > -1)
              walk(f(Data(bf)))
            else {
              close(in)
              walk(f(EOF))
            }
          case e @ Error(t) =>
            close(in)
            e
          case done @ Done(v, rest) =>
            done
        }
      }

      failover {
        walk(ait)
      }

    }
  }

  def toBuffer(in: BinProducer): Try[ByteBuffer] = {
    in(Iteratee.foldLeft(ByteBuffer.allocate(0)) { (acc, e) =>
      concat(acc, e)
    }) match {
      case Done(v, _) => Success(v)
      case Error(t)   => Failure(t)
      case k          => Failure(new IllegalStateException)
    }
  }

  def chunks(in: BinProducer): Try[Seq[ByteBuffer]] = {
    in(Iteratee.foldLeft(Nil: Seq[ByteBuffer]) { (acc, e) =>
      acc ++ List(e)
    }) match {
      case Done(v, _) => Success(v)
      case Error(t)   => Failure(t)
      case k          => Failure(new IllegalStateException)
    }
  }

  def toArray(in: BinProducer): Try[Array[Byte]] = {
    toBuffer(in) map { v =>
      val arr = new Array[Byte](v.capacity)
      v.get(arr)
      arr
    }
  }

  def concat(a: ByteBuffer, b: ByteBuffer) = {
    val nb = ByteBuffer.allocate(a.limit() + b.limit())
    nb.put(a)
    nb.put(b)
    nb.flip
    nb
  }

  def bufferToString(b: ByteBuffer): String = {
    val arr = new Array[Byte](b.limit)
    b get arr
    new String(arr, "UTF-8")
  }

  def toBuffer(in: InputStream, bufSize: Int = 32768): Try[ByteBuffer] = {
    inputStreamProducer(in, bufSize)(Iteratee.foldLeft(ByteBuffer.allocate(0)) { (acc, e) =>
      concat(acc, e)
    }) match {
      case Done(v, _) => Success(v)
      case Error(t)   => Failure(t)
      case _          => Failure(new IllegalStateException)
    }
  }

  def toString(in: BinProducer): Try[String] = toArray(in) map { new String(_, "utf-8") }

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
        case Empty   => Done(o, i)
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

