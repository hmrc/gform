package uk.gov.hmrc.gform

import play.api.http.Status
import scala.concurrent.ExecutionContext.Implicits.global

class PingSpec extends support.ITSpec {

  "The application should start and be pingable" in {

    eventually {
      wsclient.GET(s"$baseUrl/ping/ping").futureValue.status shouldBe Status.OK
    }
  }
}
