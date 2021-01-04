/*
 * Copyright 2021 HM Revenue & Customs
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

import cats.data.NonEmptyList
import java.time.LocalDate

import org.scalatest.{ FlatSpec, Matchers }
import java.time.LocalDateTime

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json._
import play.api.libs.json.Writes.DefaultLocalDateTimeWrites
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormGen
import uk.gov.hmrc.gform.sharedmodel.email.EmailConfirmationCode

class FormSpec extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  val inputJson =
    """|{
       |  "_id" : "tax",
       |  "envelopeId" : "envelopeId",
       |  "userId" : "userId",
       |  "formTemplateId" : "formTemplateId",
       |  "fields" : [ {
       |    "id" : "abc",
       |    "value" : "abc"
       |  } ],
       |  "Submitted" : { },
       |  "visitsIndex" : [ 1, 2, 3 ],
       |  "thirdPartyData" : {
       |    "obligations" : {
       |      "RetrievedObligations" : {
       |        "obligation" : [ {
       |          "id" : {
       |            "recalculatedTaxPeriodKey" : {
       |              "fcId" : "compId",
       |              "hmrcTaxPeriod" : {
       |                "idType" : {
       |                  "idType" : "eeits"
       |                },
       |                "idNumber" : {
       |                  "Value" : { }
       |                },
       |                "regimeType" : {
       |                  "regimeType" : "ITSA"
       |                }
       |              }
       |            },
       |            "idNumberValue" : {
       |              "value" : "123"
       |            }
       |          },
       |          "obligation" : {
       |            "obligations" : [ {
       |              "obligationDetails" : [ {
       |                "status" : "O",
       |                "inboundCorrespondenceFromDate" : "2100-01-15",
       |                "inboundCorrespondenceToDate" : "1900-10-28",
       |                "inboundCorrespondenceDueDate" : "2061-01-25",
       |                "periodKey" : "fxzaz"
       |              } ]
       |            } ]
       |          }
       |        } ]
       |      }
       |    },
       |    "emailVerification" : {
       |      "emailId" : {
       |        "email" : "josef@hmrc.com",
       |        "code" : "HPKHWB"
       |      }
       |    },
       |    "queryParams" : {
       |        "params" : { }
       |    },
       |    "booleanExprCache" : {
       |        "mapping": { }
       |    }
       |  },
       |  "ldt" : "2064-12-01T00:00:40"
       |}""".stripMargin

  val exampleForm = Form(
    FormId("tax"),
    EnvelopeId("envelopeId"),
    uk.gov.hmrc.gform.sharedmodel.UserId("userId"),
    FormTemplateId("formTemplateId"),
    FormData(
      List(
        FormField(
          FormComponentId("abc"),
          "abc"
        )
      )
    ),
    Submitted,
    VisitIndex(Set(1, 2, 3)),
    ThirdPartyData(
      None,
      RetrievedObligations(
        NonEmptyList.one(
          TaxResponse(
            HmrcTaxPeriodWithEvaluatedId(
              RecalculatedTaxPeriodKey(
                FormComponentId("compId"),
                HmrcTaxPeriod(IdType("eeits"), Value, RegimeType("ITSA"))),
              IdNumberValue("123")),
            Obligation(
              List(
                ObligationDetails(
                  List(
                    ObligationDetail(
                      "O",
                      LocalDate.of(2100, 1, 15),
                      LocalDate.of(1900, 10, 28),
                      LocalDate.of(2061, 1, 25),
                      "fxzaz")
                  )
                )
              )
            )
          )
        )
      ),
      Map(
        FormComponentId("emailId") -> EmailAndCode("josef@hmrc.com", EmailConfirmationCode("HPKHWB"))
      ),
      QueryParams.empty,
      None,
      BooleanExprCache.empty
    ),
    Some(EnvelopeExpiryDate(LocalDateTime.of(2064, 12, 1, 0, 0, 40)))
  )

  "Format for Form" should "read json" in {
    verifyRead(exampleForm, inputJson)
  }

  it should "fallback to empty ThirdPartyData if structure of ThirdPartyData is changed" in {

    val formWithEmptyThirdParty = exampleForm.copy(thirdPartyData = ThirdPartyData.empty)

    val incompatibleJson = inputJson.replace("hmrcTaxPeriod", "wrongName") // This simulates change in ADT which will not be backwards compatible

    verifyRead(formWithEmptyThirdParty, incompatibleJson)

  }

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
    Some(EnvelopeExpiryDate(LocalDateTime.now.plusDays(1)))
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
      "thirdPartyData" -> Json.obj(
        "obligations"       -> Json.obj("NotChecked" -> Json.obj()),
        "emailVerification" -> Json.obj(),
        "queryParams"       -> Json.obj("params" -> Json.obj()),
        "booleanExprCache"  -> Json.obj("mapping" -> Json.obj())
      )
    )
    formJsObject shouldBe expectedFormJsObject

    Form.format.reads(Form.format.writes(form)) should be(JsSuccess(form))
  }

  it should "handle inflight forms not having visitsIndex or thirdPartyData" in {
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

  it should "round trip arbitrary Forms" in {
    forAll(FormGen.formGen) { form =>
      verifyRoundTrip(form)
    }
  }
}
