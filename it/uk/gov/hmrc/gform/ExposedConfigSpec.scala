package uk.gov.hmrc.gform

import play.api.http.Status
import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.config.ExposedConfig


class ExposedConfigSpec extends support.ITSpec {

  "The application should serve exposed config" in {

    eventually {
      val response = wsclient.GET(s"$baseUrl/gform/exposed-config").futureValue
      response.status shouldBe Status.OK
      val json = Json.parse(response.body)
      val econfig = Json.reads[ExposedConfig].reads(json).get
      econfig shouldBe ExposedConfig(
        formMaxAttachmentSizeMB = 10,
        formExpiryDays = 30
      )
    }
  }

}
