package uk.gov.hmrc.gform.wshttp

import akka.actor.{ActorSystem, Terminated}
import akka.stream.ActorMaterializer
import play.api.libs.ws.WSRequest
import play.api.libs.ws.ahc.AhcWSClient
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

//If you want to use WSHttp outside play app you must provide your WSClient. Otherwise it blows up.
//See https://github.com/hmrc/http-verbs/issues/60
//Don't use it on production ('ws.close()' logic is missing)
//TODO: this is copy paste from 'test'. Maybe it's worth to extract common test configuration
object TestWSHttpIT extends WSHttp {
  override def buildRequest[A](url: String)(implicit hc: HeaderCarrier): WSRequest = ws.url(url)
  private implicit lazy val s: ActorSystem = ActorSystem()
  private implicit lazy val mat: ActorMaterializer = ActorMaterializer()
  private lazy val ws = AhcWSClient()(mat)

  def stop(): Future[Terminated] = s.terminate()
}