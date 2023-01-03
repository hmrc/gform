/*
 * Copyright 2023 HM Revenue & Customs
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

import com.typesafe.config.{ Config => TypeSafeConfig }
import org.typelevel.ci.CIString
import pureconfig._
import pureconfig.generic.ProductHint
import scala.concurrent.duration.FiniteDuration
import uk.gov.hmrc.gform.sharedmodel.config.FileExtension
import pureconfig.generic.auto._

case class AppConfig(
  appName: String,
  formExpiryDays: Int,
  formMaxAttachmentSizeMB: Int,
  formMaxAttachments: Int,
  formMaxAttachmentTotalSizeMB: Int,
  `upscan-confirmation-ttl`: FiniteDuration,
  restrictedFileExtensionList: List[String],
  submittedFormExpiryHours: Int,
  `sdes-confirmation-ttl-days`: Int
) {

  def restrictedFileExtensions: List[FileExtension] =
    restrictedFileExtensionList.map(v => FileExtension(CIString(v)))

}

object AppConfig {

  def loadOrThrow(config: TypeSafeConfig): AppConfig = {

    implicit def hint: ProductHint[AppConfig] = ProductHint(ConfigFieldMapping(CamelCase, CamelCase))
    val appConfig = ConfigSource.fromConfig(config).loadOrThrow[AppConfig]

    appConfig.formExpiryDays.verifyThat(_ > 0, s"'formExpiryDays' must be positive, was ${appConfig.formExpiryDays}")
    appConfig.submittedFormExpiryHours
      .verifyThat(_ > 0, s"'submittedFormExpiryHours' must be positive, was ${appConfig.submittedFormExpiryHours}")
    appConfig.formMaxAttachments
      .verifyThat(_ > 0, s"'formMaxAttachments' must be positive, was ${appConfig.formMaxAttachments}")
    appConfig.formMaxAttachmentSizeMB
      .verifyThat(_ > 0, s"'formMaxAttachmentSizeMB' must be positive, was ${appConfig.formMaxAttachmentSizeMB}")
    appConfig.formMaxAttachmentTotalSizeMB
      .verifyThat(_ > 0, s"'formMaxAttachmentTotalSizeMB' must be positive, was ${appConfig.formMaxAttachmentSizeMB}")

    appConfig
  }

  private implicit class VerifyThat[T](t: T) {
    def verifyThat(assertion: T => Boolean, message: String = "") =
      if (!assertion(t)) throw new AppConfigException(message)
  }

  class AppConfigException(message: String) extends IllegalArgumentException(message)
}
