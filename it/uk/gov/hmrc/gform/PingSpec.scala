package uk.gov.hmrc.gform

import play.api.http.Status


class PingSpec extends support.ITSpec {

  "The application should start and be pingable" in {

    eventually {
      wsclient.url(s"$baseUrl/ping/ping").get().futureValue.status shouldBe Status.OK
    }
  }

  private lazy val baseUrl = s"http://localhost:${port}"
}
