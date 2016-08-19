package net.shift.common

import scala.util.parsing.combinator.Parsers
import scala.annotation.tailrec

trait ShiftParsers extends Parsers {
  type Elem = Char

  val reserved = Set(';', '/', '?', ':', '@', '&', '=', '+', '$', ',')

  def capitals: Parser[String] = rep1(acceptIf(b => b >= 'A' && b <= 'Z')(err => "Not a capital character " + err)) ^^ { _ mkString }

  def digit: Parser[Int] = acceptIf(b => b >= '0' && b <= '9')(err => "Not a digit character " + err) ^^ { _ - 48 }

  def int: Parser[Int] = rep1(acceptIf(b => b >= '0' && b <= '9')(err => "Not a digit character " + err)) ^^ { _.mkString.toInt }

  def noCRLFSpace: Parser[Char] = accept(' ') | accept('\t')

  def ws = rep(noCRLFSpace)

  def str(s: String) = ws ~> acceptSeq(s) <~ ws

  def notReserved: Parser[String] = rep1(acceptIf(b => !(reserved contains b))(err => "Not a value character " + err)) ^^ { _ mkString }

  def uriValid: Parser[String] = rep1(acceptIf(b => b != ' ' && b != '\t' && b != ':')(err => "Not a value character " + err)) ^^ { _ mkString }

  def until[T](p: Parser[T], retryPInput: Boolean): Parser[String] = new Parser[String] {
    def apply(in: Input): ParseResult[String] = {

      @tailrec
      def walk(i: Input, acc: StringBuilder): (Input, StringBuilder) = {
        val inBeforeP = i
        val r = p(i)

        if (!i.atEnd && !r.successful) {
          walk(i.rest, acc + i.first)
        } else {
          if (retryPInput)
            (inBeforeP, acc)
          else
            (r.next, acc)
        }
      }

      val (i, res) = walk(in, new StringBuilder)

      if (i.atEnd) {
        if (res.isEmpty)
          Failure("No content found", i)
        else
          Success(res.toString, i)
      } else
        Success(res.toString, i)

    }
  }
}