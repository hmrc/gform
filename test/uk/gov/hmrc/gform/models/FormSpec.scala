/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import play.api.libs.json._
import uk.gov.hmrc.gform.Spec

class FormSpec extends Spec {
  val formId = FormId("my-form-id")

  val formData = FormData(UserId("TESTID"), FormTypeId("my-form-type-id"), Version("0.1.5"), "UTF-8", List(FormField(FieldId("firstName"), "Josef")))

  val form = Form(formId, formData, EnvelopeId("enve-id-asd"))

  val formFormat = implicitly[Format[Form]]

  "case class Form" should "be serialized into flat structure" in {

    val formAsJson = formFormat.writes(form)
    val expectedJson = Json.obj(
      "_id" -> "my-form-id",
      "userId" -> "TESTID",
      "formTypeId" -> "my-form-type-id",
      "version" -> "0.1.5",
      "characterSet" -> "UTF-8",
      "fields" -> Json.arr(
        Json.obj(
          "id" -> "firstName",
          "value" -> "Josef"
        )
      ),
      "envelopeId" -> "enve-id-asd"
    )

    formAsJson should be(expectedJson)

    val formFromJson = formFormat.reads(formAsJson)

    formFromJson should be(JsSuccess(form))
  }
}
