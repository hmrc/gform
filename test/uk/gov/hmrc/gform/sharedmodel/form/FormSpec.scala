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

import org.scalatest.{FlatSpec, Matchers}
import java.time.LocalDateTime

import play.api.libs.json._
import play.api.libs.json.Writes.DefaultLocalDateTimeWrites
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

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
    NotChecked
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
      "InProgress"  -> Json.obj(),
      "visitsIndex" -> Json.arr(1, 2, 3),
      "ldt"         -> form.envelopeExpiryDate.map(_.ldt).map(f _).get,
      "NotChecked"  -> Json.obj()
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
      "InProgress" -> Json.obj()
    )

    val expectedForm = form.copy(visitsIndex = VisitIndex.empty, envelopeExpiryDate = None)
    Form.format.reads(inflight) should be(JsSuccess(expectedForm))
  }

  "Json with an 'obligations' key" should "correctly be formatted to a form case class" in {
    def f(d: LocalDateTime): JsValue = DefaultLocalDateTimeWrites.writes(d)

    val inflight = Json.obj(
      "_id"            -> "James007-AAA999",
      "envelopeId"     -> "b66c5979-e885-49cd-9281-c7f42ce6b307",
      "userId"         -> "James007",
      "formTemplateId" -> "AAA999",
      "fields" -> Json
        .arr(
          Json.obj("id" -> "facePhoto", "value"      -> "face-photo.jpg"),
          Json.obj("id" -> "startDate-year", "value" -> "2008")),
      "InProgress" -> Json.obj(),
      "visitsIndex" -> Json.arr(1, 2, 3),
      "ldt"         -> form.envelopeExpiryDate.map(_.ldt).map(f _).get,
      "obligations" -> Json.arr(
          Json.obj("idNumberValue" -> Json.obj("value" -> "gfdsa"),
            "periodKey" -> "17B2",
            "hmrcTaxPeriod" -> Json.obj("idType" -> Json.obj("idType" -> "nino"),
                "idNumber" -> Json.obj("expr" -> Json.obj("FormCtx"-> Json.obj("value" -> "ho930Reg"))),
                "regimeType" -> "ITSA"),
            "inboundCorrespondenceFromDate" -> 1496271600000l,
            "inboundCorrespondenceToDate" -> 1504134000000l
            )
      )
    )

    val expectedForm = form.copy(obligations =   RetrievedObligations(List(TaxPeriodInformation(HmrcTaxPeriod(IdType("nino"),TextExpression(FormCtx("ho930Reg")),RegimeType("ITSA")),IdNumberValue("gfdsa"),new java.util.Date(1496271600000l),new java.util.Date(1504134000000l), "17B2"))))
    Form.format.reads(inflight) should be(JsSuccess(expectedForm))
  }

  "Json with a 'RetrievedObligations / listOfObligations' key" should "correctly be formatted to a form case class" in {
    def f(d: LocalDateTime): JsValue = DefaultLocalDateTimeWrites.writes(d)

    val inflight = Json.obj(
      "_id"            -> "James007-AAA999",
      "envelopeId"     -> "b66c5979-e885-49cd-9281-c7f42ce6b307",
      "userId"         -> "James007",
      "formTemplateId" -> "AAA999",
      "fields" -> Json
        .arr(
          Json.obj("id" -> "facePhoto", "value"      -> "face-photo.jpg"),
          Json.obj("id" -> "startDate-year", "value" -> "2008")),
      "InProgress" -> Json.obj(),
      "visitsIndex" -> Json.arr(1, 2, 3),
      "ldt"         -> form.envelopeExpiryDate.map(_.ldt).map(f _).get,
      "RetrievedObligations" -> Json.obj("listOfObligations" -> Json.arr(
        Json.obj("idNumberValue" -> Json.obj("value" -> "gfdsa"),
          "periodKey" -> "17B2",
          "hmrcTaxPeriod" -> Json.obj("idType" -> Json.obj("idType" -> "nino"),
            "idNumber" -> Json.obj("expr" -> Json.obj("FormCtx"-> Json.obj("value" -> "ho930Reg"))),
            "regimeType" -> "ITSA"),
          "inboundCorrespondenceFromDate" -> 1496271600000l,
          "inboundCorrespondenceToDate" -> 1504134000000l
        )
      ))
    )

    val expectedForm = form.copy(obligations =   RetrievedObligations(List(TaxPeriodInformation(HmrcTaxPeriod(IdType("nino"),TextExpression(FormCtx("ho930Reg")),RegimeType("ITSA")),IdNumberValue("gfdsa"),new java.util.Date(1496271600000l),new java.util.Date(1504134000000l), "17B2"))))
    Form.format.reads(inflight) should be(JsSuccess(expectedForm))
  }
}

