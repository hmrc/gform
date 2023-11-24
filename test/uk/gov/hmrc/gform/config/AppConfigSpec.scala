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

import com.typesafe.config.ConfigFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AppConfigSpec extends AnyFlatSpec with Matchers {

  "AppConfig" should "be loadable" in {

    val appConfig = AppConfig.loadOrThrow(ConfigFactory.load())
    appConfig.appName shouldBe "gform"
    appConfig.formExpiryDays shouldBe 28
    appConfig.createdFormExpiryDays shouldBe 90
    appConfig.submittedFormExpiryHours shouldBe 48
    appConfig.formMaxAttachments shouldBe 15
    appConfig.formMaxAttachmentSizeMB shouldBe 10
    appConfig.formMaxAttachmentTotalSizeMB shouldBe 25
    appConfig.restrictedFileExtensionList shouldBe List("jfif", "png")
  }

}
