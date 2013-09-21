package net.shift
package netty

import engine.http.Cookie
import org.jboss.netty.handler.codec.http.{ Cookie => NettyCookie };
import org.jboss.netty.handler.codec.http.HttpRequest;
import org.jboss.netty.handler.codec.http.HttpResponse;
import org.jboss.netty.handler.codec.http.QueryStringDecoder;

object NettyHttpExtractor {
  import scala.collection.JavaConversions._

  def parameters(q: QueryStringDecoder): Map[String, List[String]] = {
    val it = q.getParameters().entrySet().iterator()
    val pm = new scala.collection.mutable.LinkedHashMap[String, List[String]]()

    while (it.hasNext()) {
      val entry = it.next();
      pm += entry.getKey -> asScalaBuffer(entry.getValue()).toList
    }
    pm.toMap
  }

  def headers(r: HttpRequest): Map[String, String] = {
    val it = r.getHeaders().iterator()
    val pm = new scala.collection.mutable.LinkedHashMap[String, String]()

    while (it.hasNext()) {
      val entry = it.next();
      pm += entry.getKey -> entry.getValue()
    }
    pm.toMap
  }

  def queryString(uri: String): Option[String] = {
    val pos = uri.indexOf("?")
    if (pos < 0)
      None
    else
      Some(uri.substring(pos + 1))
  }

  def uriPath(uri: String): String = {
    val pos = uri.indexOf("?")
    if (pos < 0)
      uri
    else
      uri.substring(0, pos)
  }

  def cookiesMap(in: Option[scala.collection.mutable.Set[NettyCookie]]) = {
    in.map(set =>
      (for (nc <- set) yield {
        (nc.getName(), new Cookie(nc.getName(),
          nc.getValue(),
          Option(nc.getDomain()),
          Option(nc.getPath()),
          Option(nc.getMaxAge()),
          Option(nc.getVersion()),
          nc.isSecure(),
          nc.isHttpOnly()))
      }).toMap) getOrElse Map.empty
  }

}
