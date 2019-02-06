/*
 * Copyright 2019 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.config

import com.typesafe.config.ConfigFactory
import pureconfig.{ CamelCase, ConfigFieldMapping, ProductHint }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.config.ContentType

case class SampleConf(fooFoo: Int, bar: String)

class AppConfigSpec extends Spec {

  behavior of "AppConfig"

  it should "be loadable" in {

    val appConfig = AppConfig.loadOrThrow(ConfigFactory.load())
    appConfig.appName shouldBe "gform"
    appConfig.formExpiryDays shouldBe 28
    appConfig.formMaxAttachments shouldBe 15
    appConfig.formMaxAttachmentSizeMB shouldBe 10
    appConfig.formMaxAttachmentTotalSizeMB shouldBe 25
    appConfig.contentTypes shouldBe List(
      ContentType.`application/pdf`,
      ContentType.`image/jpeg`,
      ContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
      ContentType(".xlsx"),
      ContentType("application/vnd.oasis.opendocument.spreadsheet"),
      ContentType(".ods"),
      ContentType("application/vnd.openxmlformats-officedocument.wordprocessingml.document"),
      ContentType(".docx"),
      ContentType("application/vnd.oasis.opendocument.text"),
      ContentType(".odt"),
      ContentType("application/vnd.openxmlformats-officedocument.presentationml.presentation"),
      ContentType(".pptx"),
      ContentType("application/vnd.oasis.opendocument.presentation"),
      ContentType(".odp")
    )

  }

}
