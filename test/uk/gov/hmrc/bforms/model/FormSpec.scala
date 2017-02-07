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

package uk.gov.hmrc.bforms.model

import uk.gov.hmrc.bforms.model._
import play.api.libs.json._
import org.scalatest._

class FormSpec extends FlatSpec with Matchers with StreamlinedXml {
  val formId = FormId("my-form-id")

  val formData = FormData(FormTypeId("my-form-type-id"), "0.1.5", "UTF-8", List(FormField("firstName", "Josef")))

  val form = Form(formId, formData)

  val formFormat = implicitly[Format[Form]]

  "case class Form" should "be serialized into flat structure" in {

    val formAsJson = formFormat.writes(form)
    val expectedJson = Json.obj(
      "_id" -> "my-form-id",
      "formTypeId" -> "my-form-type-id",
      "version" -> "0.1.5",
      "characterSet" -> "UTF-8",
      "fields" -> Json.arr(
        Json.obj(
          "id" -> "firstName",
          "value" -> "Josef"
        )
      )
    )

    formAsJson should be(expectedJson)

    val formFromJson = formFormat.reads(formAsJson)

    formFromJson should be(JsSuccess(form))
  }
}
