package net.shift
package common

import net.shift.common.StringUtils._
import net.shift.io.FileSystem
import net.shift.io.IO._

import scala.annotation.tailrec
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

object Config {

  def apply(props: (String, String)*): Config = new Config(props.toMap)

  def fromString(cfg: String): Try[Config] = new ConfigParser().parse(cfg)

  /**
    * Loads the config from folder ./config/{profile}.conf
    *
    * @param profile
    * @param fs
    * @return
    */
  def load(profile: String = "")(implicit fs: FileSystem): Try[Config] = {
    for {
      in <- fs.reader(Path(s"config/$profile.conf"))
      str <- producerToString(in)
      cfg <- ConfigParser.parse(str)
    } yield {
      cfg
    }
  }

  def apply() = new Config(Map.empty)
}

class Config(val configs: Map[String, String]) {
  self =>

  def append(other: Map[String, String]): Config = new Config(self.configs ++ other)

  def int(p: String, d: Int = 0): Int = configs.get(p).flatMap(toInt(_)).getOrElse(d)

  def long(p: String, d: Long = 0): Long = configs.get(p).flatMap(toLong(_)).getOrElse(d)

  def bool(p: String, d: Boolean = false): Boolean = configs.get(p).flatMap(toBool(_)).getOrElse(d)

  def double(p: String, d: Double = 0.0): Double = configs.get(p).flatMap(toDouble(_)).getOrElse(d)

  def string(p: String, d: String = ""): String = configs.get(p).getOrElse(d)

  def list(p: String, d: List[String] = Nil): List[String] = configs.get(p).map(s => s.trim.split("\\s*,\\s*").toList).getOrElse(d)

  def +(other: Config) = new Config(configs ++ other.configs)

}

object ConfigParser {
  def parse(cfg: String): Try[Config] = new ConfigParser().parse(cfg)
}

class ConfigParser extends RegexParsers {
  override def skipWhitespace = false

  def word: Parser[String] = """\w+""".r

  def ws = rep(accept(' ') | accept('\t'))

  def ret = accept('\r') | accept('\n')

  def wss = rep(accept(' ') | accept('\t') | ret)

  def singleProp: Parser[Seq[(String, String)]] = (wss ~> word <~ ws <~ accept('=') <~ ws) ~ until(ret) ^^ {
    case k ~ v => List(k -> v)
  }

  def props: Parser[Seq[(String, String)]] = rep(singleProp | prefixed) ^^ {
    case l => l.flatten
  }

  def prefixed: Parser[Seq[(String, String)]] = (wss ~> word <~ ws <~ accept('{')) ~ (props <~ wss <~ accept('}')) ^^ {
    case l ~ r => r.map {
      case (k, v) => (l + "." + k) -> v
    }
  }


  def until[T](p: Parser[T], retryPInput: Boolean = false): Parser[String] = new Parser[String] {
    def apply(in: Input): ParseResult[String] = {

      @tailrec
      def walk(i: Input, acc: String): (Input, String) = {
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

      val (i, res) = walk(in, "")

      if (i.atEnd) {
        if (res.isEmpty)
          Failure("No content found", i)
        else
          Success(res, i)
      } else
        Success(res, i)

    }
  }

  def config: Parser[Config] = props ^^ {
    case l => new Config(l.toMap)
  }

  def parse(in: CharSequenceReader): Try[Config] = config(in) match {
    case Success(r, _) => scala.util.Success(r)
    case Failure(f, _) => scala.util.Failure(new Exception(f))
    case Error(f, _) => scala.util.Failure(new Exception(f))
  }

  def parse(cfg: String): Try[Config] = parse(new CharSequenceReader(cfg))

}
