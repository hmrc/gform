package uk.gov.hmrc.gform

import uk.gov.hmrc.gform.sharedmodel.config.{ContentType, ExposedConfig}

import scala.concurrent.ExecutionContext.Implicits.global


class ExposedConfigSpec extends support.ITSpec {

  "ExposedConfig" in eventually {

    gformConnector.getExposedConfig.futureValue shouldBe ExposedConfig(
      formMaxAttachmentSizeMB = 5,
      formExpiryDays = 30,
      List(
        ContentType.`application/pdf`,
        ContentType.`image/jpeg`
      )
    )
  }
}

