/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.formtemplate

import munit.FunSuite
import play.api.libs.json.{ JsError, JsSuccess, Json }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class AddToListExpressionHelperSuite extends FunSuite with FormTemplateSupport {

  test(
    "AddToListExpressionHelper.updateRequiredExpressionsInAtl should update exprs in smart strings and includeIfs inside/outside ATL that use ChoicesAvailable"
  ) {
    val jsonStr =
      """|{
         |  "_id": "already-selected-info",
         |  "formName": "Name of the service",
         |  "description": "",
         |  "version": 1,
         |  "emailTemplateId": "eeitt_submission_confirmation",
         |  "authConfig": {
         |    "authModule": "hmrc"
         |  },
         |  "sections": [
         |    {
         |      "id": "payments",
         |      "type": "addToList",
         |      "title": "Payments",
         |      "summaryName": "Payments",
         |      "shortName": "Payment $n",
         |      "description": "<strong>Tax year:</strong> ${taxYear}\n\n<strong>Amount:</strong> ${amount}",
         |      "summaryDescription": "<strong>Tax year:</strong> ${taxYear}\n\n<strong>Amount:</strong> ${amount}",
         |      "repeatsUntil": "${noMoreChoices(taxYear)}",
         |      "addAnotherQuestion": {
         |        "id": "addPayment",
         |        "type": "choice",
         |        "label": "Add another payment?",
         |        "errorMessage": "Select yes if you want to add another payment",
         |        "choices": [
         |          "Yes",
         |          "No"
         |        ]
         |      },
         |      "fields": [
         |        {
         |          "id": "taxYearSelectedInfo1",
         |          "type": "info",
         |          "label": "",
         |          "infoText": "You have already added ${totalChoicesSelected(taxYear)} tax years.",
         |          "infoType": "noformat"
         |        },
         |        {
         |          "id": "taxYearSelectedInfo2",
         |          "type": "info",
         |          "label": "",
         |          "includeIf": "${noMoreChoices(taxYear)}",
         |          "infoText": "There are no more tax years to add",
         |          "infoType": "noformat"
         |        }
         |      ],
         |      "pages": [
         |        {
         |          "title": "Which tax year?",
         |          "fields": [
         |            {
         |              "id": "taxYearSelectedInfo3",
         |              "type": "info",
         |              "label": "",
         |              "includeIf": "${totalChoicesSelected(taxYear) > 0}",
         |              "infoText": "You have already added ${totalChoicesSelected(taxYear)} tax years.",
         |              "infoType": "noformat"
         |            },
         |            {
         |              "id": "taxYearSelectedInfo4",
         |              "type": "info",
         |              "label": "",
         |              "includeIf": "${totalChoicesSelected(taxYear) = 0}",
         |              "infoText": "You can only select each of the ${choicesCount(taxYear)} tax years once.",
         |              "infoType": "noformat"
         |            },
         |            {
         |              "type": "choice",
         |              "id": "taxYear",
         |              "label": "Select tax year",
         |              "labelSize": "s",
         |              "hideChoicesSelected": true,
         |              "notPII": true,
         |              "multivalue": true,
         |              "choices": [
         |                {
         |                  "en": "2023 to 24",
         |                  "value": "2024",
         |                  "includeIf": "${year(TODAY) <= 2024}"
         |                },
         |                {
         |                  "en": "2024 to 25",
         |                  "value": "2025",
         |                  "includeIf": "${year(TODAY) <= 2025}"
         |                },
         |                {
         |                  "en": "2025 to 26",
         |                  "value": "2026"
         |                },
         |                {
         |                  "en": "2026 to 27",
         |                  "value": "2027"
         |                },
         |                {
         |                  "en": "2027 to 28",
         |                  "value": "2028"
         |                }
         |              ]
         |            }
         |          ]
         |        },
         |        {
         |          "title": "What payment did you make in ${taxYear}?",
         |          "fields": [
         |            {
         |              "id": "amount",
         |              "type": "text",
         |              "format": "sterling"
         |            }
         |          ]
         |        }
         |      ]
         |    }
         |  ],
         |  "acknowledgementSection": {
         |    "title": "Acknowledgement Page",
         |    "fields": [
         |      {
         |        "id": "nextSteps",
         |        "type": "info",
         |        "label": "",
         |        "infoText": "We have sent you a confirmation email.\n\n[Print or save a PDF copy of your form](${link.printAcknowledgementPdf})\n## What happens next",
         |        "infoType": "noformat"
         |      }
         |    ]
         |  },
         |  "destinations": [
         |    {
         |      "id": "transitionToSubmitted",
         |      "type": "stateTransition",
         |      "requiredState": "Submitted"
         |    }
         |  ]
         |}""".stripMargin

    val ft = toFormTemplate(jsonStr)
    val res = AddToListExpressionHelper.updateRequiredExpressionsInAtl(ft)
    res.formKind.allSections.foreach(s =>
      s.fold(_ => None)(_ => None) { atl =>
        atl.fields.get.map {
          case f @ IsInformationMessage(info) =>
            if (f.id.value == "taxYearSelectedInfo2") {
              //TEST - insideAtl should be false as this is repeater page
              f.includeIf.get.booleanExpr shouldBe Equals(
                ChoicesAvailable(FormComponentId("taxYear"), Some(false)),
                Constant("0")
              )
              None
            } else {
              //TEST - insideAtl should be false as this is repeater page
              info.infoText.internals.head.interpolations.head shouldBe Subtraction(
                ChoicesCount(FormComponentId("taxYear")),
                ChoicesAvailable(FormComponentId("taxYear"), Some(false))
              )
              None
            }
          case _ => None
        }
        atl.pages.map(p =>
          p.fields.map {
            case f @ IsInformationMessage(info) =>
              if (f.id.value == "taxYearSelectedInfo3") {
                //TESTS - insideAtl should be true as this is child page of ATL
                f.includeIf.get.booleanExpr shouldBe GreaterThan(
                  Subtraction(
                    ChoicesCount(FormComponentId("taxYear")),
                    ChoicesAvailable(FormComponentId("taxYear"), Some(true))
                  ),
                  Constant("0")
                )
                info.infoText.internals.head.interpolations.head shouldBe Subtraction(
                  ChoicesCount(FormComponentId("taxYear")),
                  ChoicesAvailable(FormComponentId("taxYear"), Some(true))
                )
              }
            case _ => None
          }
        )
        None
      }
    )
  }

  private def toFormTemplate(jsonStr: String): FormTemplate = {
    val maybeNormalisedJson = FormTemplatesControllerRequestHandler.normaliseJSON(Json.parse(jsonStr))
    maybeNormalisedJson match {
      case JsError(error) => fail(s"Failed to normalise json $error")
      case JsSuccess(normalisedJson, _) =>
        normalisedJson.as[FormTemplate]
    }
  }
}
