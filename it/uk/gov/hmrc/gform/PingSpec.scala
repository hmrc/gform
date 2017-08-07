package uk.gov.hmrc.gform

import play.api.http.Status
import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.config.ExposedConfig


class PingSpec extends support.ITSpec {

  "The application should start and be pingable" in {

    eventually {
      wsclient.GET(s"$baseUrl/ping/ping").futureValue.status shouldBe Status.OK
    }
  }
}
