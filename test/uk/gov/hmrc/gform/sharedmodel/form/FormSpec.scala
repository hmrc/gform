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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Writes.DefaultLocalDateTimeWrites
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.email.EmailConfirmationCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormGen

import java.time.temporal.ChronoUnit
import java.time.{ Instant, LocalDate, LocalDateTime }

class FormSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  val nowInst = Instant.now.truncatedTo(ChronoUnit.MILLIS)

  val inputJson =
    s"""|{
        |  "_id" : "tax",
        |  "envelopeId" : "envelopeId",
        |  "userId" : "userId",
        |  "formTemplateId" : "formTemplateId",
        |  "fields" : [ {
        |    "id" : "abc",
        |    "value" : "abc"
        |  } ],
        |  "Submitted" : { },
        |  "visitsIndex" : [ "n1", "n2", "n3" ],
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
        |  "ldt" : "2064-12-01T00:00:40",
        |  "startDate" : {
        |    "$$date" : {
        |      "$$numberLong" : "${nowInst.toEpochMilli.toString}"
        |    }
        |  }
        |}""".stripMargin

  val exampleForm = Form(
    FormId("tax"),
    EnvelopeId("envelopeId"),
    uk.gov.hmrc.gform.sharedmodel.UserId("userId"),
    FormTemplateId("formTemplateId"),
    None,
    FormData(
      List(
        FormField(
          FormComponentId("abc"),
          "abc"
        )
      )
    ),
    Submitted,
    VisitIndex.Classic(
      (
        Set(
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(1)),
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(2)),
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(3))
        )
      )
    ),
    ThirdPartyData(
      RetrievedObligations(
        NonEmptyList.one(
          TaxResponse(
            HmrcTaxPeriodWithEvaluatedId(
              RecalculatedTaxPeriodKey(
                FormComponentId("compId"),
                HmrcTaxPeriod(IdType("eeits"), Value, RegimeType("ITSA"))
              ),
              IdNumberValue("123")
            ),
            Obligation(
              List(
                ObligationDetails(
                  List(
                    ObligationDetail(
                      "O",
                      LocalDate.of(2100, 1, 15),
                      LocalDate.of(1900, 10, 28),
                      LocalDate.of(2061, 1, 25),
                      "fxzaz"
                    )
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
      BooleanExprCache.empty,
      None,
      None,
      None,
      None,
      None,
      None
    ),
    Some(EnvelopeExpiryDate(LocalDateTime.of(2064, 12, 1, 0, 0, 40))),
    FormComponentIdToFileIdMapping.empty,
    TaskIdTaskStatusMapping.empty,
    nowInst
  )

  "Format for Form" should "read json" in {
    verifyRead(exampleForm, inputJson)
  }

  it should "fallback to empty ThirdPartyData if structure of ThirdPartyData is changed" in {

    val formWithEmptyThirdParty = exampleForm.copy(thirdPartyData = ThirdPartyData.empty)

    val incompatibleJson = inputJson.replace(
      "hmrcTaxPeriod",
      "wrongName"
    ) // This simulates change in ADT which will not be backwards compatible

    verifyRead(formWithEmptyThirdParty, incompatibleJson)

  }

  val form = Form(
    FormId("James007-AAA999"),
    EnvelopeId("b66c5979-e885-49cd-9281-c7f42ce6b307"),
    uk.gov.hmrc.gform.sharedmodel.UserId("James007"),
    FormTemplateId("AAA999"),
    None,
    FormData(
      List(
        FormField(FormComponentId("facePhoto"), "face-photo.jpg"),
        FormField(FormComponentId("startDate-year"), "2008")
      )
    ),
    InProgress,
    VisitIndex.Classic(
      (
        Set(
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(1)),
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(2)),
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(3))
        )
      )
    ),
    ThirdPartyData.empty,
    Some(EnvelopeExpiryDate(LocalDateTime.now.plusDays(1))),
    FormComponentIdToFileIdMapping.empty,
    TaskIdTaskStatusMapping.empty,
    nowInst
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
          Json.obj("id" -> "startDate-year", "value" -> "2008")
        ),
      "InProgress"  -> Json.obj(),
      "visitsIndex" -> Json.arr("n1", "n2", "n3"),
      "ldt"         -> form.envelopeExpiryDate.map(_.ldt).map(f _).get,
      "thirdPartyData" -> Json.obj(
        "obligations"       -> Json.obj("NotChecked" -> Json.obj()),
        "emailVerification" -> Json.obj(),
        "queryParams"       -> Json.obj("params" -> Json.obj()),
        "booleanExprCache"  -> Json.obj("mapping" -> Json.obj())
      ),
      "componentIdToFileId" -> Json.obj(),
      "taskIdTaskStatus"    -> Json.obj(),
      "startDate" -> Json.obj(
        "$date" -> Json.obj(
          "$numberLong" -> nowInst.toEpochMilli.toString
        )
      )
    )
    formJsObject shouldBe expectedFormJsObject

    Form.format.reads(Form.format.writes(form)) should be(JsSuccess(form))
  }

  it should "handle inflight forms not having thirdPartyData" in {
    val inflight = Json.obj(
      "_id"            -> "James007-AAA999",
      "envelopeId"     -> "b66c5979-e885-49cd-9281-c7f42ce6b307",
      "userId"         -> "James007",
      "formTemplateId" -> "AAA999",
      "fields" -> Json
        .arr(
          Json.obj("id" -> "facePhoto", "value"      -> "face-photo.jpg"),
          Json.obj("id" -> "startDate-year", "value" -> "2008")
        ),
      "visitsIndex" -> Json.arr(),
      "InProgress"  -> Json.obj(),
      "startDate" -> Json.obj(
        "$date" -> Json.obj(
          "$numberLong" -> nowInst.toEpochMilli.toString
        )
      )
    )

    val expectedForm = form.copy(visitsIndex = VisitIndex.empty(FormKind.Classic(Nil)), envelopeExpiryDate = None)
    Form.format.reads(inflight) should be(JsSuccess(expectedForm))
  }

  it should "round trip arbitrary Forms" in {
    forAll(FormGen.formGen) { form =>
      verifyRoundTrip(form)
    }
  }

  "FormComponentIdToFileIdMapping" should "be flat when serialized" in {
    val componentIdToFileId = FormComponentIdToFileIdMapping(
      Map(
        FileComponentId.Single(FormComponentId("1_abc")) -> FileId("invoice.pdf"),
        FileComponentId.Single(FormComponentId("2_abc")) -> FileId("book.pdf")
      )
    )

    verifyRoundTrip(componentIdToFileId)
    val json = Json.toJson(componentIdToFileId)
    val expected = Json.obj(
      "1_abc" -> "invoice.pdf",
      "2_abc" -> "book.pdf"
    )
    json shouldBe expected

  }
}
