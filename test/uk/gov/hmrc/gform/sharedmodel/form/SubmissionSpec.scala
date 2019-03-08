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

import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.{ExampleData, RetrievedObligations, TaxPeriodInformation}
import uk.gov.hmrc.gform.submission.Submission

class SubmissionSpec extends Spec {

  "case class Submission" should "be serialized into json" in new ExampleData {

    override val formFields = super.formFields.take(2)

    val expecteJsonString =
      """
      {
        "_id": "James007-AAA999",
        "submittedDate": "2007-12-03T10:15:30",
        "submissionRef": "DMS",
        "envelopeId": "b66c5979-e885-49cd-9281-c7f42ce6b307",
        "attachment_count": 0,
        "formTemplateId": "AAA999",
        "customerId": "TESTNINO"
      }
      """.stripMargin

    val expectedJson = Json.parse(expecteJsonString)

    Submission.format.writes(submission) shouldBe expectedJson

    Submission.format.reads(expectedJson) shouldBe JsSuccess(submission)
  }


  "asda" should " dsad" in {
    val a  = """{
               |  "_id" : "testGroupId-feb9773b-ba82-412a-8b52-e31593aeef90-def",
               |  "envelopeId" : "9100925c-1d9e-4e8b-ade4-500207a71314",
               |  "userId" : "testGroupId-feb9773b-ba82-412a-8b52-e31593aeef90",
               |  "formTemplateId" : "def",
               |  "fields" : [ {
               |    "id" : "ho930Reg",
               |    "value" : "dsadas"
               |  }, {
               |    "id" : "businessName",
               |    "value" : "sadasdasda"
               |  }, {
               |    "id" : "itsaReturnTaxPeriod",
               |    "value" : "17B2|2017-06-01|2017-08-31"
               |  }, {
               |    "id" : "itsaReturnTaxPeriod2",
               |    "value" : "17B2|2017-06-01|2017-08-31"
               |  }, {
               |    "id" : "totalDue",
               |    "value" : "0"
               |  } ],
               |  "InProgress" : { },
               |  "visitsIndex" : [ 0, 1, 2 ],
               |  "ldt" : "2019-04-05T12:06:31.329",
               |  "RetrievedObligations" : {
               |    "listOfObligations" : [ {
               |      "idNumberValue" : {
               |        "value" : "dsadas"
               |      },
               |      "periodKey" : "17B2",
               |      "hmrcTaxPeriod" : {
               |        "idType" : {
               |          "idType" : "nino"
               |        },
               |        "idNumber" : {
               |          "expr" : {
               |            "FormCtx" : {
               |              "value" : "ho930Reg"
               |            }
               |          }
               |        },
               |        "regimeType" : {
               |          "regimeType" : "ITSA"
               |        }
               |      },
               |      "inboundCorrespondenceFromDate" : 1496271600000,
               |      "inboundCorrespondenceToDate" : 1504134000000
               |    }, {
               |      "idNumberValue" : {
               |        "value" : "dsadas"
               |      },
               |      "periodKey" : "16AH",
               |      "hmrcTaxPeriod" : {
               |        "idType" : {
               |          "idType" : "nino"
               |        },
               |        "idNumber" : {
               |          "expr" : {
               |            "FormCtx" : {
               |              "value" : "ho930Reg"
               |            }
               |          }
               |        },
               |        "regimeType" : {
               |          "regimeType" : "ITSA"
               |        }
               |      },
               |      "inboundCorrespondenceFromDate" : 1470006000000,
               |      "inboundCorrespondenceToDate" : 1472598000000
               |    }, {
               |      "idNumberValue" : {
               |        "value" : "sadasdasda"
               |      },
               |      "periodKey" : "17B2",
               |      "hmrcTaxPeriod" : {
               |        "idType" : {
               |          "idType" : "nino"
               |        },
               |        "idNumber" : {
               |          "expr" : {
               |            "FormCtx" : {
               |              "value" : "businessName"
               |            }
               |          }
               |        },
               |        "regimeType" : {
               |          "regimeType" : "VATC"
               |        }
               |      },
               |      "inboundCorrespondenceFromDate" : 1496271600000,
               |      "inboundCorrespondenceToDate" : 1504134000000
               |    }, {
               |      "idNumberValue" : {
               |        "value" : "sadasdasda"
               |      },
               |      "periodKey" : "16AH",
               |      "hmrcTaxPeriod" : {
               |        "idType" : {
               |          "idType" : "nino"
               |        },
               |        "idNumber" : {
               |          "expr" : {
               |            "FormCtx" : {
               |              "value" : "businessName"
               |            }
               |          }
               |        },
               |        "regimeType" : {
               |          "regimeType" : "VATC"
               |        }
               |      },
               |      "inboundCorrespondenceFromDate" : 1470006000000,
               |      "inboundCorrespondenceToDate" : 1472598000000
               |    } ]
               |  }
               |}""".stripMargin
//    val a = """[{"idNumberValue":{"value":"dfghjk"},"periodKey":"17B2","hmrcTaxPeriod":{"idType":{"idType":"nino"},"idNumber":{"expr":{"FormCtx":{"value":"ho930Reg"}}},"regimeType":{"regimeType":"ITSA"}},"inboundCorrespondenceFromDate":1496271600000,"inboundCorrespondenceToDate":1504134000000},{"idNumberValue":{"value":"dfghjk"},"periodKey":"16AH","hmrcTaxPeriod":{"idType":{"idType":"nino"},"idNumber":{"expr":{"FormCtx":{"value":"ho930Reg"}}},"regimeType":{"regimeType":"ITSA"}},"inboundCorrespondenceFromDate":1470006000000,"inboundCorrespondenceToDate":1472598000000}]"""
    val b  = Json.parse(a).asOpt[Form]
    println("asdasd" + b)
    println("ccacaacaca" + b.get.obligations)
    b.get.obligations.isInstanceOf[RetrievedObligations] shouldBe true
  }
}
