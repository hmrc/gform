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

package uk.gov.hmrc.gform.sharedmodel.form

import java.time.LocalDateTime

import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.ExampleData

class FormSpec extends Spec {

  "case class Form" should "be serialized into json" in new ExampleData {
    override val formFields = super.formFields.filter(f => Set("facePhoto", "startDate-year").contains(f.id.value))

    val formJsObject: JsObject = Form.format.writes(form)

    def f(d:LocalDateTime) : String = {
      val dd = d
      val s = EnvelopeExpiryDate.dateTimeFormat.format(d)
      val ss = s
      s
    }
    val expectedFormJsObject = Json.obj(
      "_id"            -> "James007-AAA999",
      "envelopeId"     -> "b66c5979-e885-49cd-9281-c7f42ce6b307",
      "userId"         -> "James007",
      "formTemplateId" -> "AAA999",
      "fields" -> Json
        .arr(
          Json.obj("id" -> "facePhoto", "value"      -> "face-photo.jpg"),
          Json.obj("id" -> "startDate-year", "value" -> "2008")),
      "InProgress" -> Json.obj(),
      "ldt"        -> form.envelopeExpiryDate.map(_.ldt).map(f _).get
    )

    val xxx = LocalDateTime.now.plusDays(1)
    println(xxx)
    println(Json.prettyPrint(formJsObject))
    println(Json.prettyPrint(expectedFormJsObject))


    val s = formJsObject.toString()
    val e = expectedFormJsObject.toString()
    val y =1
    val yy = y

    //formJsObject shouldBe expectedFormJsObject

    val res1 = Json.toJson(xxx)

    val res2 = res1.asOpt[LocalDateTime]




    println("xxx " + xxx)
      println("res1 " + res1)
    println("res2 " + res2)


    Form.format.reads(Form.format.writes(form)) should be(JsSuccess(form))
  }
}
