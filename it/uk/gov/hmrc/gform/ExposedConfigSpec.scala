package uk.gov.hmrc.gform

import uk.gov.hmrc.gform.sharedmodel.config.{ContentType, ExposedConfig}

import scala.concurrent.ExecutionContext.Implicits.global


class ExposedConfigSpec extends support.ITSpec {

  "ExposedConfig" in eventually {

    gformConnector.getExposedConfig.futureValue shouldBe ExposedConfig(
      formMaxAttachmentSizeMB = 10,
      formExpiryDays = 30,
      List(
        ContentType.`application/pdf`,
        ContentType.`application/xml; charset=UTF-8`,
        ContentType.`image/jpeg`,
        ContentType.`text/plain`,
        ContentType.`application/vnd.ms-excel`,
        ContentType.`application/vnd.openxmlformats-officedocument.spreadsheetml.sheet`
      )
    )
  }
}

