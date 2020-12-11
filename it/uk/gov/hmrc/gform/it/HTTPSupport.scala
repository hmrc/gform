package uk.gov.hmrc.gform.it

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Seconds, Span }
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import uk.gov.hmrc.gform.it.HTTPSupport.{ Empty, Request }

trait HTTPSupport extends ScalaFutures {

  implicit val defaultPatience: PatienceConfig =
    PatienceConfig(timeout = Span(10, Seconds), interval = Span(500, Millis))

  val request: Request = Empty

  def post(body: String): HTTPSupport = newHTTPSupport(this.request.copy(body = body, method = "POST"))

  def put(body: String): HTTPSupport = newHTTPSupport(this.request.copy(body = body, method = "PUT"))

  def get(url: String): HTTPSupport = newHTTPSupport(this.request.copy(url = url, method = "GET"))

  def delete(url: String): HTTPSupport = newHTTPSupport(this.request.copy(url = url, method = "DELETE"))

  def to(url: String): HTTPSupport = newHTTPSupport(this.request.copy(url = url))

  private def newHTTPSupport(updatedRequest: Request): HTTPSupport = new HTTPSupport {
    override val request: Request = updatedRequest
  }

  def send()(implicit baseUrl: String, wsClient: StandaloneAhcWSClient) = {
    val wsRequest = wsClient
      .url(baseUrl + request.url)
      .withHttpHeaders("Content-Type" -> "application/json")

    request.method match {
      case "GET" =>
        wsRequest.get().futureValue
      case "POST" =>
        wsRequest.post(request.body).futureValue
      case "PUT" =>
        wsRequest.put(request.body).futureValue
      case "DELETE" =>
        wsRequest.delete().futureValue
      case other => throw new UnsupportedOperationException(s"Method $other not implemented")
    }
  }
}

object HTTPSupport {
  case class Request(body: String = "", url: String = "", method: String = "")
  val Empty: Request = Request()
}
