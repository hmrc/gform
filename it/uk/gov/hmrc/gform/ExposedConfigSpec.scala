package uk.gov.hmrc.gform

import uk.gov.hmrc.gform.sharedmodel.config.ExposedConfig
import scala.concurrent.ExecutionContext.Implicits.global


class ExposedConfigSpec extends support.ITSpec {

  "ExposedConfig" in eventually {

    gformConnector.getExposedConfig.futureValue shouldBe ExposedConfig(
      formMaxAttachmentSizeMB = 10,
      formExpiryDays = 30
    )
  }
}
