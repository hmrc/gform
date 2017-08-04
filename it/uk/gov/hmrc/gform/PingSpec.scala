package uk.gov.hmrc.gform

import play.api.http.Status


class PingSpec extends support.ITSpec {

  "The application should start and be pingable" in {

    eventually {
      wsclient.GET(s"$baseUrl/ping/ping").futureValue.status shouldBe Status.OK
    }
  }

  private lazy val baseUrl = s"http://localhost:${port}"
}
