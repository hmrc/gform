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

package uk.gov.hmrc.gform.formtemplate

import munit.FunSuite
import play.api.libs.json.{ JsError, JsSuccess, Json }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class MiniSummaryListHelperSuite extends FunSuite with FormTemplateSupport {

  test(
    "MiniSummaryListHelper.updateAllMslIncludeIfs should update includeIfs on summarySection MSL from pageId and taskId refs"
  ) {
    val jsonStr =
      """|{
         |  "_id": "hidden-tasks-pages-summary-list",
         |  "formName": "Name of service",
         |  "description": "",
         |  "version": 1,
         |  "emailTemplateId": "eeitt_submission_confirmation",
         |  "authConfig": {
         |    "authModule": "anonymous"
         |  },
         |  "sections": [
         |    {
         |      "title": "Applicant details",
         |      "tasks": [
         |        {
         |          "id": "task1",
         |          "title": "Type of applicant",
         |          "sections": [
         |            {
         |              "title": "Select what type of applicant you are",
         |              "fields": [
         |                {
         |                  "id": "applicantType",
         |                  "type": "choice",
         |                  "choices": [
         |                    "Limited Company",
         |                    "Sole trader"
         |                  ]
         |                }
         |              ]
         |            },
         |            {
         |              "title": "What is your revenue?",
         |              "fields": [
         |                {
         |                  "id": "revenue",
         |                  "type": "text",
         |                  "format": "sterling"
         |                }
         |              ]
         |            }
         |          ],
         |          "summarySection": {
         |            "title": "Check your answers",
         |            "header": "",
         |            "footer": ""
         |          }
         |        },
         |        {
         |          "id": "task2",
         |          "title": "Limited company details",
         |          "includeIf": "${applicantType contains 0}",
         |          "sections": [
         |            {
         |              "title": "Select what type of applicant you are",
         |              "fields": [
         |                {
         |                  "id": "companyName",
         |                  "type": "text",
         |                  "format": "text"
         |                }
         |              ]
         |            },
         |            {
         |              "id": "profitAmount",
         |              "title": "What is your profit?",
         |              "includeIf": "${revenue > 100000}",
         |              "fields": [
         |                {
         |                  "id": "profit",
         |                  "type": "text",
         |                  "format": "sterling"
         |                }
         |              ]
         |            }
         |          ],
         |          "summarySection": {
         |            "title": "Check your answers",
         |            "header": "",
         |            "footer": ""
         |          }
         |        }
         |      ]
         |    }
         |  ],
         |  "submitSection": {
         |    "label": "Check and send to HMRC",
         |    "taskLabel": "Submit"
         |  },
         |  "summarySection": {
         |    "title": {
         |      "en": "Check your answers",
         |      "cy": "Gwiriwch eich atebion"
         |    },
         |    "fields": [
         |      {
         |        "id": "miniCya1",
         |        "type": "miniSummaryList",
         |        "label": "sdfsdf",
         |        "rows": [
         |          {
         |            "key": "Limited company details",
         |            "value": "${companyName}",
         |            "taskId": "task2"
         |          },
         |          {
         |            "key": "Profit",
         |            "value": "'number2'",
         |            "pageId": "profitAmount",
         |            "includeIf": "${profit > 50000}"
         |          }
         |        ]
         |      }
         |    ],
         |    "hideDefaultRows": true,
         |    "displayWidth": "xl",
         |    "header": "",
         |    "footer": "Before you do this you can [print or save a PDF copy of your answers (opens in a new window or tab)](${link.printSummaryPdf})."
         |  },
         |  "acknowledgementSection": {
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
    val res = MiniSummaryListHelper.updateAllMslIncludeIfs(ft)
    val summarySection = res.summarySection
    summarySection.fields.head.head.`type` match {
      case MiniSummaryList(rows, _, _) =>
        rows.foreach {
          case MiniSummaryRow.ValueRow(_, _, includeIf, pageIdOpt, taskIdOpt) =>
            (pageIdOpt, taskIdOpt) match {
              case (Some(_), None) =>
                includeIf shouldBe Some(
                  new IncludeIf(
                    And(
                      And(
                        GreaterThan(FormCtx(FormComponentId("revenue")), Constant("100000")),
                        Contains(FormCtx(FormComponentId("applicantType")), Constant("0"))
                      ),
                      GreaterThan(FormCtx(FormComponentId("profit")), Constant("50000"))
                    )
                  )
                )
              case (None, Some(_)) =>
                includeIf shouldBe Some(
                  new IncludeIf(Contains(FormCtx(FormComponentId("applicantType")), Constant("0")))
                )
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }
  }

  test(
    "MiniSummaryListHelper.updateAllMslIncludeIfs should update includeIfs on a page's MSL from pageId and taskId refs"
  ) {
    val jsonStr =
      """|{
         |  "_id": "hidden-tasks-pages-summary-list",
         |  "formName": "Name of service",
         |  "description": "",
         |  "version": 1,
         |  "emailTemplateId": "eeitt_submission_confirmation",
         |  "authConfig": {
         |    "authModule": "anonymous"
         |  },
         |  "sections": [
         |    {
         |      "title": "Applicant details",
         |      "tasks": [
         |        {
         |          "id": "task1",
         |          "title": "Type of applicant",
         |          "sections": [
         |            {
         |              "title": "Select what type of applicant you are",
         |              "fields": [
         |                {
         |                  "id": "applicantType",
         |                  "type": "choice",
         |                  "choices": [
         |                    "Limited Company",
         |                    "Sole trader"
         |                  ]
         |                }
         |              ]
         |            },
         |            {
         |              "title": "What is your revenue?",
         |              "fields": [
         |                {
         |                  "id": "revenue",
         |                  "type": "text",
         |                  "format": "sterling"
         |                }
         |              ]
         |            }
         |          ],
         |          "summarySection": {
         |            "title": "Check your answers",
         |            "header": "",
         |            "footer": ""
         |          }
         |        },
         |        {
         |          "id": "task2",
         |          "title": "Limited company details",
         |          "includeIf": "${applicantType contains 0}",
         |          "sections": [
         |            {
         |              "title": "Select what type of applicant you are",
         |              "fields": [
         |                {
         |                  "id": "companyName",
         |                  "type": "text",
         |                  "format": "text"
         |                }
         |              ]
         |            },
         |            {
         |              "id": "profitAmount",
         |              "title": "What is your profit?",
         |              "includeIf": "${revenue > 100000}",
         |              "fields": [
         |                {
         |                  "id": "profit",
         |                  "type": "text",
         |                  "format": "sterling"
         |                }
         |              ]
         |            },
         |            {
         |              "title": "MSL",
         |              "id": "mlsPage",
         |              "fields": [
         |                {
         |                  "id": "mslField",
         |                  "type": "miniSummaryList",
         |                  "label": "sdfsdf",
         |                  "rows": [
         |                    {
         |                      "key": "Applicant details",
         |                      "value": "${choice(applicantType)} - ${revenue}",
         |                      "taskId": "task1"
         |                    },
         |                    {
         |                      "key": "Limited company details",
         |                      "value": "${companyName}",
         |                      "taskId": "task2"
         |                    },
         |                    {
         |                      "key": "Profit",
         |                      "value": "'number2'",
         |                      "pageId": "profitAmount",
         |                      "includeIf": "${profit > 50000}"
         |                    }
         |                  ]
         |                }
         |              ]
         |            }
         |          ]
         |        }
         |      ]
         |    }
         |  ],
         |  "submitSection": {
         |    "label": "Check and send to HMRC",
         |    "taskLabel": "Submit"
         |  },
         |  "summarySection": {
         |    "title": {
         |      "en": "Check your answers",
         |      "cy": "Gwiriwch eich atebion"
         |    },
         |    "displayWidth": "xl",
         |    "header": "",
         |    "footer": "Before you do this you can [print or save a PDF copy of your answers (opens in a new window or tab)](${link.printSummaryPdf})."
         |  },
         |  "acknowledgementSection": {
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
    val res = MiniSummaryListHelper.updateAllMslIncludeIfs(ft)
    res.formKind.allSections.foreach(s =>
      s.fold(nrp =>
        nrp.page.id match {
          case Some(value) =>
            if (value.id == "mlsPage") {
              nrp.page.fields.map {
                case IsMiniSummaryList(summaryList) =>
                  summaryList.rows.foreach {
                    case MiniSummaryRow.ValueRow(_, _, includeIf, pageIdOpt, taskIdOpt) =>
                      (pageIdOpt, taskIdOpt) match {
                        case (Some(_), None) =>
                          includeIf shouldBe Some(
                            new IncludeIf(
                              And(
                                And(
                                  GreaterThan(FormCtx(FormComponentId("revenue")), Constant("100000")),
                                  Contains(FormCtx(FormComponentId("applicantType")), Constant("0"))
                                ),
                                GreaterThan(FormCtx(FormComponentId("profit")), Constant("50000"))
                              )
                            )
                          )
                        case (None, Some(_)) =>
                          includeIf shouldBe Some(
                            new IncludeIf(Contains(FormCtx(FormComponentId("applicantType")), Constant("0")))
                          )
                        case _ =>
                      }
                    case _ =>
                  }
                case _ => None
              }
            }
          case None =>
        }
      )(_ => None)(_ => None)
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
