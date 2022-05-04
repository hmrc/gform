/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.fileupload

import play.api.libs.json.Json
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.sharedmodel.ExampleData

class HelperSpec extends Spec {

  "helper.createEnvelopeRequestBody" should "be compatible with fileuplod expectations" in new ExampleData
    with ExampleFileUploadData {
    val date = form.envelopeExpiryDate.map(_.ldt).get
    val helper = new Helper(config)
    val allowedFileTypes = FileInfoConfig.allAllowedFileTypes
    helper.createEnvelopeRequestBody(formTemplateId, allowedFileTypes, date) shouldBe Json.obj(
      "constraints" -> Json.obj(
        "contentTypes" -> Json.arr(
          "application/pdf",
          "image/jpeg",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          "application/vnd.oasis.opendocument.spreadsheet",
          "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
          "application/vnd.oasis.opendocument.text",
          "application/vnd.openxmlformats-officedocument.presentationml.presentation",
          "application/vnd.oasis.opendocument.presentation"
        ),
        "maxItems"       -> 3,
        "maxSize"        -> "20MB",
        "maxSizePerItem" -> "5MB"
      ),
      "expiryDate" -> helper.envelopeExpiryDate(date),
      "metadata"   -> Json.obj("application" -> "gform", "formTemplateId" -> "aaa999")
    )
  }
}
