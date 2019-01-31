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

package uk.gov.hmrc.gform.fileupload

import play.api.libs.json.Json
import play.api.libs.json.Writes.DefaultLocalDateTimeWrites
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeExpiryDate

class HelperSpec extends Spec {

  "helper.createEnvelopeRequestBody" should "be compatible with fileuplod expectations" in new ExampleData
  with ExampleFileUploadData {
    val date = form.envelopeExpiryDate.map(_.ldt).get
    val helper = new Helper(config)
    helper.createEnvelopeRequestBody(formTemplateId, date) shouldBe Json.obj(
      "constraints" -> Json.obj(
        "contentTypes" -> Json.arr(
          "application/pdf",
          "application/xml",
          "image/jpeg",
          "text/xml",
          "application/vnd.ms-excel",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        ),
        "maxItems"       -> 3,
        "maxSize"        -> "20MB",
        "maxSizePerItem" -> "5MB"
      ),
      "expiryDate" -> s"${helper.envelopeExpiryDate(date)}",
      "metadata"   -> Json.obj("application" -> "gform", "formTemplateId" -> "AAA999")
    )
  }

}
