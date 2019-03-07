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

import org.scalatest.{ FlatSpec, Matchers }
import play.api.libs.json.Writes.DefaultLocalDateTimeWrites
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.NotChecked
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParameters, FormComponentId, FormTemplateId }

class FormSpec extends FlatSpec with Matchers {

  val form = Form(
    FormId("James007-AAA999"),
    EnvelopeId("b66c5979-e885-49cd-9281-c7f42ce6b307"),
    uk.gov.hmrc.gform.sharedmodel.UserId("James007"),
    FormTemplateId("AAA999"),
    FormData(
      List(
        FormField(FormComponentId("facePhoto"), "face-photo.jpg"),
        FormField(FormComponentId("startDate-year"), "2008")
      )
    ),
    InProgress,
    VisitIndex(Set(1, 2, 3)),
    ThirdPartyData.empty,
    Some(EnvelopeExpiryDate(LocalDateTime.now.plusDays(1))),
    NotChecked,
    EmailParameters(Map("test" -> "test"))
  )

  "case class Form" should "be serialized into json" in {

    val formJsObject: JsObject = Form.format.writes(form)

    def f(d: LocalDateTime): JsValue = DefaultLocalDateTimeWrites.writes(d)

    val expectedFormJsObject = Json.obj(
      "_id"            -> "James007-AAA999",
      "envelopeId"     -> "b66c5979-e885-49cd-9281-c7f42ce6b307",
      "userId"         -> "James007",
      "formTemplateId" -> "AAA999",
      "fields" -> Json
        .arr(
          Json.obj("id" -> "facePhoto", "value"      -> "face-photo.jpg"),
          Json.obj("id" -> "startDate-year", "value" -> "2008")),
      "InProgress"      -> Json.obj(),
      "visitsIndex"     -> Json.arr(1, 2, 3),
      "ldt"             -> form.envelopeExpiryDate.map(_.ldt).map(f _).get,
      "NotChecked"      -> Json.obj(),
      "emailParameters" -> Json.obj("test" -> "test")
    )
    formJsObject shouldBe expectedFormJsObject
    Form.format.reads(Form.format.writes(form)) should be(JsSuccess(form))
  }

  it should "handle inflight forms not having visitsIndex or obligations" in {
    val inflight = Json.obj(
      "_id"            -> "James007-AAA999",
      "envelopeId"     -> "b66c5979-e885-49cd-9281-c7f42ce6b307",
      "userId"         -> "James007",
      "formTemplateId" -> "AAA999",
      "fields" -> Json
        .arr(
          Json.obj("id" -> "facePhoto", "value"      -> "face-photo.jpg"),
          Json.obj("id" -> "startDate-year", "value" -> "2008")),
      "InProgress"      -> Json.obj(),
      "emailParameters" -> Json.obj("test" -> "test")
    )

    val expectedForm = form.copy(visitsIndex = VisitIndex.empty, envelopeExpiryDate = None)
    Form.format.reads(inflight) should be(JsSuccess(expectedForm))
  }

}
