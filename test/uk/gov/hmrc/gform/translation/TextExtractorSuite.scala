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

package uk.gov.hmrc.gform.translation

import io.circe.CursorOp._
import io.circe.literal.JsonStringContext
import io.circe.parser._
import java.io.{ BufferedOutputStream, ByteArrayOutputStream }
import munit.FunSuite
import Instruction._
import TextExtractor.gformPaths
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Constant

class TextExtractorSuite extends FunSuite {
  test("Annotate top level field") {
    val json =
      json"""
          {
            "formName": "there"
          }
          """
    val expectedJson =
      json"""
          {
            "formName": "✅"
          }
          """

    val res = Translator(json, gformPaths).translateJsonDebug
    assertEquals(res, expectedJson)
  }

  test("Annotate nested field") {
    val json =
      json"""
          {
            "summarySection": {
              "title": "foo",
              "header": "bar"
            }
          }
          """
    val expectedJson =
      json"""
          {
            "summarySection": {
              "title": "✅",
              "header": "✅"
            }
          }
          """

    val res = Translator(json, gformPaths).translateJsonDebug
    assertEquals(res, expectedJson)
  }

  test("Annotate array fields") {
    val json =
      json"""
          {
            "exitPages": [
              {
                "if": "$${booleanExpression}",
                "label": "Page unavailable",
                "exitMessage": "Page is unavailable until next week"
              },
              {
                "if": "$${booleanExpression2}",
                "label": "Page unavailable 2",
                "exitMessage": "Page is unavailable until next month"
              }
            ]
          }
          """
    val expectedJson =
      json"""
          {
            "exitPages": [
              {
                "if": "$${booleanExpression}",
                "label": "✅",
                "exitMessage": "✅"
              },
              {
                "if": "$${booleanExpression2}",
                "label": "✅",
                "exitMessage": "✅"
              }
            ]
          }
          """

    val res = Translator(json, gformPaths).translateJsonDebug
    assertEquals(res, expectedJson)
  }

  test("resolveInstruction") {
    val json =
      json"""
          {
            "foo": [
              {
                "bar": "bazA"
              },
              {
                "bar": "bazB"
              },
              {
                "bar": "bazC"
              },
              {
                "bar": "bazD"
              }
            ]
          }
          """
    val instructions = List(
      Pure(DownField("bar")),
      TraverseArray,
      Pure(DownField("foo"))
    )
    val expectedInstructions =
      List(
        ReductionInProgress.Wrapper(Pure(DownField("bar"))),
        ReductionInProgress.DeepTraverse(4),
        ReductionInProgress.Wrapper(Pure(DownArray)),
        ReductionInProgress.Wrapper(Pure(DownField("foo")))
      )
    val res = Translator.resolveInstruction(instructions, json.hcursor)
    assertEquals(res, expectedInstructions)
  }

  test("eliminateDeepTraverse (1)") {
    val instructions =
      List(
        ReductionInProgress.Wrapper(Pure(DownField("foo"))),
        ReductionInProgress.DeepTraverse(4),
        ReductionInProgress.Wrapper(Pure(DownArray)),
        ReductionInProgress.Wrapper(Pure(DownField("bar")))
      )

    // format: off
    val expected: List[List[Instruction]] = List(
      List(Pure(DownField("foo")),                                                    Pure(DownArray), Pure(DownField("bar"))),
      List(Pure(DownField("foo")),                                   Pure(MoveRight), Pure(DownArray), Pure(DownField("bar"))),
      List(Pure(DownField("foo")),                  Pure(MoveRight), Pure(MoveRight), Pure(DownArray), Pure(DownField("bar"))),
      List(Pure(DownField("foo")), Pure(MoveRight), Pure(MoveRight), Pure(MoveRight), Pure(DownArray), Pure(DownField("bar")))
    )
    // format: on

    val res: List[List[Instruction]] = Translator.eliminateDeepTraverse(instructions)

    assertEquals(res, expected)
  }

  test("eliminateDeepTraverse (2)") {
    val instructions =
      List(
        ReductionInProgress.Wrapper(Pure(DownField("label"))),
        ReductionInProgress.Wrapper(TraverseArray),
        ReductionInProgress.Wrapper(Pure(DownArray)),
        ReductionInProgress.Wrapper(Pure(DownField("fields"))),
        ReductionInProgress.DeepTraverse(3),
        ReductionInProgress.Wrapper(Pure(DownArray)),
        ReductionInProgress.Wrapper(Pure(DownField("sections")))
      )
    // format: off
    val expected: List[List[Instruction]] = List(
      List(Pure(DownField("label")), TraverseArray, Pure(DownArray), Pure(DownField("fields")),                                   Pure(DownArray), Pure(DownField("sections"))),
      List(Pure(DownField("label")), TraverseArray, Pure(DownArray), Pure(DownField("fields")),                  Pure(MoveRight), Pure(DownArray), Pure(DownField("sections"))),
      List(Pure(DownField("label")), TraverseArray, Pure(DownArray), Pure(DownField("fields")), Pure(MoveRight), Pure(MoveRight), Pure(DownArray), Pure(DownField("sections")))
    )
    // format: on

    val res: List[List[Instruction]] = Translator.eliminateDeepTraverse(instructions)

    assertEquals(res, expected)
  }

  test("annotateJson (1)") {

    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "label": "What is your email"
                  },
                  {
                    "label": "Sic code"
                  },
                  {
                    "label": "Currency"
                  }
                ]
              },
              {
                "fields": [
                  {
                    "label": "Lower A passenger number"
                  },
                  {
                    "label": "Standard A passenger number"
                  }
                ]
              },
              {
                "fields": [
                  {
                    "label": "Higher A passenger number"
                  }
                ]
              }
            ]
          }
          """

    val expected =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "label": "✅"
                  },
                  {
                    "label": "✅"
                  },
                  {
                    "label": "✅"
                  }
                ]
              },
              {
                "fields": [
                  {
                    "label": "✅"
                  },
                  {
                    "label": "✅"
                  }
                ]
              },
              {
                "fields": [
                  {
                    "label": "✅"
                  }
                ]
              }
            ]
          }
          """

    val res = Translator(json, gformPaths).translateJsonDebug
    assertEquals(res, expected)
  }

  test("annotateJson (2)") {
    val json =
      json"""
          {
            "sections": [
              {
                "tasks": [
                  {
                    "sections": [
                      {
                        "fields": [
                          {
                            "choices": [
                              "Yes",
                              "No",
                              "Another VAT number"
                            ]
                          },
                          {
                            "choices": [
                              "Yes",
                              "No"
                            ]
                          }
                        ]
                      }
                    ]
                  }
                ]
              }
            ]
          }
          """
    val expected =
      json"""
          {
            "sections": [
              {
                "tasks": [
                  {
                    "sections": [
                      {
                        "fields": [
                          {
                            "choices": [
                              "✅",
                              "✅",
                              "✅"
                            ]
                          },
                          {
                            "choices": [
                              "✅",
                              "✅"
                            ]
                          }
                        ]
                      }
                    ]
                  }
                ]
              }
            ]
          }
          """

    val res = Translator(json, gformPaths).translateJsonDebug
    assertEquals(res, expected)
  }

  test("Extract rows (1)") {
    val json =
      json"""
          {
            "hey": "there"
          }
          """

    val instructions = List(
      List(
        Pure(DownField("hey"))
      )
    )

    val expectedRows = List(
      Row(".hey", "there", "")
    )

    val res = Translator(json, instructions).fetchRows

    assertEquals(res, expectedRows)
  }

  test("Extract rows (2)") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "label": "What is your email"
                  },
                  {
                    "label": "Sic code",
                    "choices": [
                      "Choice 1",
                      "Choice 2",
                      "Choice 3"
                    ]
                  }
                ]
              },
              {
                "fields": [
                  {
                    "label": "Lower A passenger number",
                    "choices": [
                      "Choice 1",
                      "Choice 2",
                      "Choice 3"
                    ]
                  }
                ]
              }
            ]
          }
          """

    val instructions =
      List(
        List(
          TraverseArray,
          Pure(DownField("choices")),
          TraverseArray,
          Pure(DownField("fields")),
          TraverseArray,
          Pure(DownField("sections"))
        )
      )

    val expectedRows = List(
      Row(".sections[0].fields[1].choices[0]", "Choice 1", ""),
      Row(".sections[0].fields[1].choices[1]", "Choice 2", ""),
      Row(".sections[0].fields[1].choices[2]", "Choice 3", ""),
      Row(".sections[1].fields[0].choices[0]", "Choice 1", ""),
      Row(".sections[1].fields[0].choices[1]", "Choice 2", ""),
      Row(".sections[1].fields[0].choices[2]", "Choice 3", "")
    )

    val res = Translator(json, instructions).fetchRows

    assertEquals(res, expectedRows)
  }

  test("Extract rows (3)") {
    val json =
      json"""
          {
            "hey": [
              [
                {
                  "foo": "bar"
                }
              ],
              [
                {
                  "foo": "baz"
                }
              ]
            ]
          }
          """

    val instructions =
      List(
        List(
          Pure(DownField("foo")),
          TraverseArray,
          TraverseArray,
          Pure(DownField("hey"))
        )
      )

    val expectedRows = List(
      Row(".hey[0][0].foo", "bar", ""),
      Row(".hey[1][0].foo", "baz", "")
    )

    val res = Translator(json, instructions).fetchRows

    assertEquals(res, expectedRows)
  }

  test("Extract rows (4)") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "revealingFields": [
                      [
                        {
                          "infoText": "bar"
                        }
                      ],
                      [
                        {
                          "infoText": "baz"
                        }
                      ]
                    ]
                  }
                ]
              }
            ]
          }
          """

    val instructions =
      List(
        List(
          Pure(DownField("infoText")),
          TraverseArray,
          TraverseArray,
          Pure(DownField("revealingFields")),
          TraverseArray,
          Pure(DownField("fields")),
          TraverseArray,
          Pure(DownField("sections"))
        )
      )

    val expectedRows = List(
      Row(".sections[0].fields[0].revealingFields[0][0].infoText", "bar", ""),
      Row(".sections[0].fields[0].revealingFields[1][0].infoText", "baz", "")
    )

    val res = Translator(json, instructions).fetchRows

    assertEquals(res, expectedRows)
  }

  test("Extract rows (5) - preserve white spaces at the beginning of the content") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                       "en": "\n* Austria \n* Belgium \n* Bulgaria"
                    }
                  }
                ]
              }
            ]
          }
          """

    val instructions =
      List(
        List(
          TraverseArray,
          Pure(DownField("infoText")),
          TraverseArray,
          Pure(DownField("fields")),
          TraverseArray,
          Pure(DownField("sections"))
        )
      )

    val expectedRows = List(
      Row(".sections[0].fields[0].infoText", "\n* Austria \n* Belgium \n* Bulgaria", "")
    )

    val res = Translator(json, instructions).fetchRows

    assertEquals(res, expectedRows)
  }

  test("Extract rows (6) - preserve white spaces at the beginning of the content") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": "\n* Austria \n* Belgium \n* Bulgaria"
                  }
                ]
              }
            ]
          }
          """

    val instructions =
      List(
        List(
          TraverseArray,
          Pure(DownField("infoText")),
          TraverseArray,
          Pure(DownField("fields")),
          TraverseArray,
          Pure(DownField("sections"))
        )
      )

    val expectedRows = List(
      Row(".sections[0].fields[0].infoText", "\n* Austria \n* Belgium \n* Bulgaria", "")
    )

    val res = Translator(json, instructions).fetchRows

    assertEquals(res, expectedRows)
  }

  test("Choices with values") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "choices": [
                      {
                        "value": "nestedFoo",
                        "en": "Address",
                        "cy": "Nested Iawn"
                      },
                      {
                        "value": "nestedBar",
                        "en": "Date",
                        "cy": "Nested Iawn"
                      }
                    ]
                  }
                ]
              }
            ]
          }
          """

    val instructions =
      List(
        List(
          TraverseArray,
          Pure(DownField("choices")),
          TraverseArray,
          Pure(DownField("fields")),
          TraverseArray,
          Pure(DownField("sections"))
        )
      )

    val expectedRows =
      List(
        Row(".sections[0].fields[0].choices[0]", "Address", "Nested Iawn"),
        Row(".sections[0].fields[0].choices[1]", "Date", "Nested Iawn")
      )

    val res = Translator(json, instructions).fetchRows

    assertEquals(res, expectedRows)

  }

  test("Choices with hint and summaryValue") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "label": "What is your email"
                  },
                  {
                    "label": "Sic code",
                    "choices": [
                      {
                        "en" : "Choice 1",
                        "hint": "Choice 1 hint",
                        "summaryValue": "Choice 1 summary value"
                      },
                      {
                        "en" : "Choice 2",
                        "hint": "Choice 2 hint",
                        "summaryValue": "Choice 2 summary value"
                      }
                    ]
                  }
                ]
              }
            ]
          }
          """

    val expectedRows = List(
      Row(".sections[0].fields[0].label", "What is your email", ""),
      Row(".sections[0].fields[1].choices[0]", "Choice 1", ""),
      Row(".sections[0].fields[1].choices[0].hint", "Choice 1 hint", ""),
      Row(".sections[0].fields[1].choices[0].summaryValue", "Choice 1 summary value", ""),
      Row(".sections[0].fields[1].choices[1]", "Choice 2", ""),
      Row(".sections[0].fields[1].choices[1].hint", "Choice 2 hint", ""),
      Row(".sections[0].fields[1].choices[1].summaryValue", "Choice 2 summary value", ""),
      Row(".sections[0].fields[1].label", "Sic code", "")
    )

    val res = Translator(json, TextExtractor.gformPaths).fetchRows

    assertEquals(res, expectedRows)
  }

  test("generateExpressionOpHistory") {
    val json =
      json"""
          {
            "expressions": {
              "vesselInstallationExp": "foo",
              "vesselInstallationHelptextExp": "foo",
              "euNationalExp": "foo",
              "employeesYour": "foo"
            }
          }
          """
    val expectedOps = List(
      List(DownField("vesselInstallationExp"), DownField("expressions")),
      List(DownField("vesselInstallationHelptextExp"), DownField("expressions")),
      List(DownField("euNationalExp"), DownField("expressions")),
      List(DownField("employeesYour"), DownField("expressions"))
    )

    val res = TopLevelExprData.generateExpressionOpHistory(json)
    assertEquals(res, expectedOps)
  }

  test("Translate json (1)") {
    val json =
      json"""
          {
            "hey": "there"
          }
          """

    val expectedJson =
      json"""
          {
            "hey": {
              "en": "there",
              "cy": "cy-there"
            }
          }
          """

    val rows = List(
      Row(".hey", "there", "cy-there")
    )

    val instructions = List(
      List(
        Pure(DownField("hey"))
      )
    )

    val res = Translator(json, instructions).translateJson(rows)

    assertEquals(res, expectedJson)
  }

  test("Translate json (2)") {
    val json =
      json"""
          {
            "hey": [
              [
                {
                  "foo": "bar"
                }
              ],
              [
                {
                  "foo": "baz"
                }
              ]
            ]
          }
          """

    val expectedJson =
      json"""
          {
            "hey": [
              [
                {
                  "foo": {
                    "en": "bar",
                    "cy": "cy-bar"
                  }
                }
              ],
              [
                {
                  "foo": {
                    "en": "baz",
                    "cy": "cy-baz"
                  }
                }
              ]
            ]
          }
          """

    val rows = List(
      Row(".hey[0][0].foo", "bar", "cy-bar"),
      Row(".hey[1][0].foo", "baz", "cy-baz")
    )

    val instructions =
      List(
        List(
          Pure(DownField("foo")),
          TraverseArray,
          TraverseArray,
          Pure(DownField("hey"))
        )
      )
    val res = Translator(json, instructions).translateJson(rows)

    assertEquals(res, expectedJson)
  }

  test("Translate json (3)") {
    val json =
      json"""
          {
            "hey": [
              [
                {
                  "foo": "bar"
                }
              ],
              [
                {
                  "foo": "baz"
                }
              ]
            ]
          }
          """

    val expectedJson =
      json"""
          {
            "hey": [
              [
                {
                  "foo": {
                    "en": "bar",
                    "cy": "cy-bar"
                  }
                }
              ],
              [
                {
                  "foo": {
                    "en": "baz",
                    "cy": "cy-baz"
                  }
                }
              ]
            ]
          }
          """

    val rows = List(
      Row(".hey[0][0].foo", "bar", "cy-bar"),
      Row(".hey[1][0].foo", "baz", "cy-baz")
    )

    val instructions =
      List(
        List(
          Pure(DownField("foo")),
          TraverseArray,
          TraverseArray,
          Pure(DownField("hey"))
        )
      )
    val res = Translator(json, instructions).translateJson(rows)

    assertEquals(res, expectedJson)
  }

  test("Translate json (4)") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "choices": [
                      {
                        "value": "nestedFoo",
                        "en": "Address"
                      },
                      {
                        "value": "nestedBar",
                        "en": "Date"
                      }
                    ]
                  }
                ]
              }
            ]
          }
          """
    val expectedJson =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "choices": [
                      {
                        "value": "nestedFoo",
                        "en": "Address",
                        "cy": "Nested Iawn"
                      },
                      {
                        "value": "nestedBar",
                        "en": "Date",
                        "cy": "Nested Iawn"
                      }
                    ]
                  }
                ]
              }
            ]
          }
          """

    val instructions =
      List(
        List(
          TraverseArray,
          Pure(DownField("choices")),
          TraverseArray,
          Pure(DownField("fields")),
          TraverseArray,
          Pure(DownField("sections"))
        )
      )

    val rows =
      List(
        Row(".sections[0].fields[0].choices[0]", "Address", "Nested Iawn"),
        Row(".sections[0].fields[0].choices[1]", "Date", "Nested Iawn")
      )

    val res = Translator(json, instructions).translateJson(rows)

    assertEquals(res, expectedJson)

  }

  test("Translate json (5)") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "choices": [
                      {
                        "value": "nestedFoo",
                        "en": "Address",
                        "cy": "Nested Iawn"
                      },
                      {
                        "value": "nestedBar",
                        "en": "Date",
                        "cy": "Nested Iawn"
                      }
                    ]
                  }
                ]
              }
            ]
          }
          """

    val instructions =
      List(
        List(
          TraverseArray,
          Pure(DownField("choices")),
          TraverseArray,
          Pure(DownField("fields")),
          TraverseArray,
          Pure(DownField("sections"))
        )
      )

    val rows =
      List(
        Row(".sections[0].fields[0].choices[0]", "Address", "Nested Iawn"),
        Row(".sections[0].fields[0].choices[1]", "Date", "Nested Iawn")
      )

    val res = Translator(json, instructions).translateJson(rows)

    assertEquals(res, json)

  }

  test("XX Translate json".ignore) {
    val json =
      json"""
          {
            "expressions": {
              "employeesYourCap": "if employee || agent then 'Your'|'Cy-Your' + ' ' else 'Employee’s'"
            }
          }
          """
    val expectedJson =
      json"""
          {
            "expressions": {
              "employeesYourCap": "if employee || agent then 'Your'|'Cy-Your-2' + ' ' else 'Employee’s'|'Cy-Employee’s'"
            }
          }
          """

    val instructions =
      List(
        List(
          Pure(DownField("employeesYourCap")),
          Pure(DownField("expressions"))
        )
      )

    val rows =
      List(
        Row(".expressions.employeesYourCap", "Your", "Cy-Your-2"),
        Row(".expressions.employeesYourCap", "Employee’s", "Cy-Employee’s")
      )

    val res = Translator(json, instructions).translateJson(rows)

    assertEquals(res, expectedJson)

  }

  test("rowsForTranslation") {
    val json =
      json"""
          {
            "hey": "there",
            "hou": "there",
            "foo": "",
            "bar": "$${expression}",
            "baz": "$${expression} $${expression}"
          }
          """
    val instructions = List(
      List(Pure(DownField("hey"))),
      List(Pure(DownField("hou"))),
      List(Pure(DownField("foo"))),
      List(Pure(DownField("bar"))),
      List(Pure(DownField("baz")))
    )

    val expected = List(
      TranslatedRow("there", "")
    )
    val res = Translator(json, instructions).rowsForTranslation
    assertEquals(res, expected)
  }

  test("pathWithTranslatableConstants") {
    import TranslatableConstant._
    val json =
      json"""
          {
            "expressions": {
              "vesselInstallationExp": "if empVesselInstallChoice contains 0 then 'vessel' else 'installation'",
              "vesselInstallationHelptextExp": "if empVesselInstallChoice contains 0  then  'For example, a passenger cruise ship.' else 'For example, an oil rig.'",
              "euNationalExp": "if empNationalChoice contains 0 then '0 also' else ''",
              "email": "if auth.emailLogin && user.affinityGroup != 'agent' then emailAddressGG else if auth.emailLogin && user.affinityGroup = 'agent' then emailAgent else auth.email",
              "employeesYour": "if employee || agent then 'your' else 'the employee’s'",
              "liveLives": "if employee || agent then 'live' else 'lives'",
              "employeesYourCap": "if employee || agent then 'Your'|'Cy-Your' + ' ' else 'Employee’s '"
            }
          }
          """

    // format: off
    val expected = TopLevelExprData(
      List(
        PathWithTranslatableConstants(List("vesselInstallationExp", "expressions").map(DownField), List(NonTranslated(Constant("vessel")), NonTranslated(Constant("installation")))),
        PathWithTranslatableConstants(List("vesselInstallationHelptextExp", "expressions").map(DownField), List(NonTranslated(Constant("For example, a passenger cruise ship.")), NonTranslated(Constant("For example, an oil rig.")))),
        PathWithTranslatableConstants(List("euNationalExp", "expressions").map(DownField), List(NonTranslated(Constant("0 also")))),
        PathWithTranslatableConstants(List("employeesYour", "expressions").map(DownField), List(NonTranslated(Constant("your")), NonTranslated(Constant("the employee’s")))),
        PathWithTranslatableConstants(List("liveLives", "expressions").map(DownField), List(NonTranslated(Constant("live")), NonTranslated(Constant("lives")))),
        PathWithTranslatableConstants(List("employeesYourCap", "expressions").map(DownField), List(Translated(Constant("Your"), Constant("Cy-Your")), NonTranslated(Constant("Employee’s "))))
      )
    )
    // format: on

    val res = TopLevelExprData.from(json)

    assertEquals(res, expected)
  }

  test("translate top level expression") {
    val json =
      json"""
          {
            "expressions": {
              "vesselInstallationExp": "if empVesselInstallChoice contains 0 then 'vessel' else 'installation'",
              "vesselInstallationHelptextExp": "if empVesselInstallChoice contains 0  then  'For example, a passenger cruise ship.' else 'For example, an oil rig.'",
              "euNationalExp": "if empNationalChoice contains 0 then '0 also' else ''",
              "email": "if auth.emailLogin && user.affinityGroup != 'agent' then emailAddressGG else if auth.emailLogin && user.affinityGroup = 'agent' then emailAgent else auth.email",
              "employeesYour": "if employee || agent then 'your' else 'the employee’s'",
              "liveLives": "if employee || agent then 'live' else 'lives'",
              "employeesYourCap": "if employee || agent then 'Your'|'Cy-Your' + ' ' else 'Employee’s '",
              "emptyWelsh": "if employee || agent then 'Your'|'' + ' ' else 'Employee’s '"
            }
          }
          """

    val expected = List(
      Row(".expressions.vesselInstallationExp", "vessel", ""),
      Row(".expressions.vesselInstallationExp", "installation", ""),
      Row(".expressions.vesselInstallationHelptextExp", "For example, a passenger cruise ship.", ""),
      Row(".expressions.vesselInstallationHelptextExp", "For example, an oil rig.", ""),
      Row(".expressions.euNationalExp", "0 also", ""),
      Row(".expressions.employeesYour", "your", ""),
      Row(".expressions.employeesYour", "the employee’s", ""),
      Row(".expressions.liveLives", "live", ""),
      Row(".expressions.liveLives", "lives", ""),
      Row(".expressions.employeesYourCap", "Your", "Cy-Your"),
      Row(".expressions.employeesYourCap", "Employee’s ", ""),
      Row(".expressions.emptyWelsh", "Your", ""),
      Row(".expressions.emptyWelsh", "Employee’s ", "")
    )

    val translator = Translator(json, List.empty[List[Instruction]])
    val res = translator.topLevelExprData.toRows

    assertEquals(res, expected)
  }

  test("csv generation") {
    val json =
      json"""
          {
            "expressions": {
              "vesselInstallationExp": "if empVesselInstallChoice contains 0 then 'vessel' else 'installation'",
              "employeesYourCap": "if employee || agent then 'Your'|'Cy-Your' + ' ' else 'Employee’s'",
              "enBasicGrossedTax": {
                "value": "enBasicValueBenefit * (enBasicRate / 100) * (100 / (100 - enBasicRate))",
                "type": "sterling",
                "round": "HalfUp"
              }
            },
            "exitPages": [
              {
                "label": {
                  "en": "Page unavailable",
                  "cy": "CY-Page unavailable"
                },
                "exitMessage": "Page is unavailable until next week"
              }
            ]
          }
          """
    val expectedCsv =
      """|en,cy
         |Employee’s,
         |Page is unavailable until next week,
         |Page unavailable,CY-Page unavailable
         |Your,Cy-Your
         |installation,
         |vessel,
         |""".stripMargin.replaceAll("\n", "\r\n")

    val baos = new ByteArrayOutputStream()
    val bos = new BufferedOutputStream(baos)
    TextExtractor.generateTranslatableCvsFromString(json.spaces2, bos)
    val csv = new String(baos.toByteArray())
    bos.close()
    baos.close()
    assertEquals(csv, expectedCsv)
  }

  test("csv ingestion") {
    val json =
      json"""
          {
            "expressions": {
              "vesselInstallationExp": "if empVesselInstallChoice contains 0 then 'vessel' else 'installation'",
              "employeesYourCap": "if employee || agent then 'Your'|'Cy-Your' + ' ' else 'Employee’s'",
              "enBasicGrossedTax": {
                "value": "enBasicValueBenefit * (enBasicRate / 100) * (100 / (100 - enBasicRate))",
                "type": "sterling",
                "round": "HalfUp"
              }
            },
            "exitPages": [
              {
                "label": "Page unavailable",
                "exitMessage": "Page is unavailable until next week"
              }
            ]
          }
          """

    val expected =
      json"""
          {
            "expressions": {
              "vesselInstallationExp": "if empVesselInstallChoice contains 0 then 'vessel'|'cy-vessel' else 'installation'|'cy-installation'",
              "employeesYourCap": "if employee || agent then 'Your'|'Cy-Your' + ' ' else 'Employee’s'|'Cy-Employee’s'",
              "enBasicGrossedTax": {
                "value": "enBasicValueBenefit * (enBasicRate / 100) * (100 / (100 - enBasicRate))",
                "type": "sterling",
                "round": "HalfUp"
              }
            },
            "exitPages": [
              {
                "label": {
                  "en": "Page unavailable",
                  "cy": "CY-Page unavailable"
                },
                "exitMessage": {
                  "en": "Page is unavailable until next week",
                  "cy": "CY-Page is unavailable until next week"
                }
              }
            ]
          }
          """

    val csv =
      """|en,cy
         |vessel,cy-vessel
         |installation,cy-installation
         |Your,Cy-Your
         |Employee’s,Cy-Employee’s
         |Page unavailable,CY-Page unavailable
         |Page is unavailable until next week,CY-Page is unavailable until next week
         |""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("top level expression translation with whitespaces") {
    val json =
      json"""
          {
            "expressions": {
              "emptyWelsh": "if 1 = 1 then 'vessel'|'' else 'installation'|''",
              "emptyWelshWithWhiteSpaces": "if 1 = 1 then 'Your' | '' + ' ' else 'Employee’s'  |  ''"
            }
          }
          """

    val expected =
      json"""
          {
            "expressions": {
              "emptyWelsh": "if 1 = 1 then 'vessel'|'cy-vessel' else 'installation'|'cy-installation'",
              "emptyWelshWithWhiteSpaces": "if 1 = 1 then 'Your'|'Cy-Your' + ' ' else 'Employee’s'|'Cy-Employee’s'"
            }
          }
          """

    val csv =
      """|en,cy
         |vessel,cy-vessel
         |installation,cy-installation
         |Your,Cy-Your
         |Employee’s,Cy-Employee’s
         |""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("top level expression with duplicate english content") {
    val json =
      json"""
          {
            "expressions": {
              "heading": "if prevFc contains 0 then ('EN1') else if prevFc contains 1 then ('EN2') else if prevFc contains 2 then ('EN3') else ('EN1')"
            }
          }
          """

    val expected =
      json"""
          {
            "expressions": {
              "heading": "if prevFc contains 0 then ('EN1'|'CY1') else if prevFc contains 1 then ('EN2'|'CY2') else if prevFc contains 2 then ('EN3'|'CY3') else ('EN1'|'CY1')"
            }
          }
          """

    val csv =
      """|en,cy
         |EN1,CY1
         |EN2,CY2
         |EN3,CY3
         |""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("top level expression white space preservation") {
    val json =
      json"""
          {
            "expressions": {
              "heading": "if prevFc contains 0 then '\n \nEN1: \n' else '\nEN2: '"
            }
          }
          """

    val expected =
      json"""
          {
            "expressions": {
              "heading": "if prevFc contains 0 then '\n \nEN1: \n'|'\n \nCY1: \n' else '\nEN2: '|'\nCY2: '"
            }
          }
          """

    val csv =
      """|en,cy
         |EN1:,CY1:
         |EN2:,CY2:
         |""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("top level expression white space preservation - idempotence (1)") {

    val json =
      json"""
          {
            "expressions": {
              "heading": "if prevFc contains 0 then '\n\nEN1: '|'\n\nCY1: ' else '\nEN2: '|'\nCY2: '"
            }
          }
          """

    val expected =
      json"""
          {
            "expressions": {
              "heading": "if prevFc contains 0 then '\n\nEN1: '|'\n\nCY1: ' else '\nEN2: '|'\nCY2: '"
            }
          }
          """

    val csv =
      """|en,cy
         |EN1:,CY1:
         |EN2:,CY2:
         |""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("top level expression white space preservation - idempotence (2)") {

    val json =
      json"""
          {
            "expressions": {
              "heading": "if prevFc contains 0 then '\n\nEN1: '|'\n\nCY1: ' else '\nEN2: '|'\nCY2: '"
            }
          }
          """

    val expected =
      json"""
          {
            "expressions": {
              "heading": "if prevFc contains 0 then '\n\nEN1: '|'\n\nCY1: ' else '\nEN2: '|'\nCY2: '"
            }
          }
          """

    val csv =
      """|en,cy
         |EN3:,CY3:
         |EN4:,CY4:
         |""".stripMargin // Nothing match by purpose

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("top level expression with markdown like data (1)") {

    val json =
      json"""
          {
            "expressions": {
              "heading": "if bporNotApplicable then '- Ineligible' else '* Ineligible'"
            }
          }
          """

    val expected =
      json"""
          {
            "expressions": {
              "heading": "if bporNotApplicable then '- Ineligible'|'- CY1' else '* Ineligible'|'* CY1'"
            }
          }
          """

    val csv =
      """|en,cy
         |Ineligible,CY1
         |""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("top level expression with markdown like data (2) - replacement") {

    val json =
      json"""
          {
            "expressions": {
              "heading": "if bporNotApplicable then '- IneligibleX'|'- CY1' else '* IneligibleY'|'* CY2'"
            }
          }
          """

    val expected =
      json"""
          {
            "expressions": {
              "heading": "if bporNotApplicable then '- IneligibleX'|'- CY3' else '* IneligibleY'|'* CY4'"
            }
          }
          """

    val csv =
      """|en,cy
         |IneligibleX,CY3
         |IneligibleY,CY4
         |""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("translate json with markdown (2)") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "\n* Austria \n* Belgium \n* Bulgaria \n* Croatia \n* Republic of Cyprus \n* Czech Republic"
                    }
                  }
                ]
              }
            ]
          }"""

    val expected =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "\n* Austria \n* Belgium \n* Bulgaria \n* Croatia \n* Republic of Cyprus \n* Czech Republic",
                      "cy": "\n* Awstria \n* Gwlad Belg \n* Bwlgaria \n* Croatia \n* Gweriniaeth Cyprus \n* Y Weriniaeth Tsiec"
                    }
                  }
                ]
              }
            ]
          }
          """

    val csv =
      """|en,cy
         |Austria,Awstria
         |Belgium,Gwlad Belg
         |Bulgaria,Bwlgaria
         |Croatia,Croatia
         |Republic of Cyprus,Gweriniaeth Cyprus
         |Czech Republic,Y Weriniaeth Tsiec""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("do not translate markdown with 'missing' (ie. empty) translation") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "\n* Austria \n* Belgium \n* Bulgaria \n* Croatia \n* Republic of Cyprus \n* Czech Republic"
                    }
                  }
                ]
              }
            ]
          }"""

    val expected =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "\n* Austria \n* Belgium \n* Bulgaria \n* Croatia \n* Republic of Cyprus \n* Czech Republic",
                      "cy": ""
                    }
                  }
                ]
              }
            ]
          }
          """

    val csv =
      """|en,cy
         |Austria,
         |Belgium,Gwlad Belg
         |Bulgaria,Bwlgaria
         |Croatia,Croatia
         |Republic of Cyprus,Gweriniaeth Cyprus
         |Czech Republic,Y Weriniaeth Tsiec""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("do not translate markdown with 'incomplete' translation") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "\n* Austria \n* Belgium \n* Bulgaria \n* Croatia \n* Republic of Cyprus \n* Czech Republic"
                    }
                  }
                ]
              }
            ]
          }"""

    val expected =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "\n* Austria \n* Belgium \n* Bulgaria \n* Croatia \n* Republic of Cyprus \n* Czech Republic",
                      "cy": ""
                    }
                  }
                ]
              }
            ]
          }
          """

    val csv =
      """|en,cy
         |Belgium,Gwlad Belg
         |Bulgaria,Bwlgaria
         |Croatia,Croatia
         |Republic of Cyprus,Gweriniaeth Cyprus
         |Czech Republic,Y Weriniaeth Tsiec""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("translate expression containing whole sentence") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "Include the types of income generated in each country$${if 1 = 1 then ', such as business profits or dividends.' else '. Acceptable income types are business profits or pensions.'}"
                    }
                  }
                ]
              }
            ]
          }"""

    val expected =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "Include the types of income generated in each country$${if 1 = 1 then ', such as business profits or dividends.' else '. Acceptable income types are business profits or pensions.'}",
                      "cy": "CY1"
                    }
                  }
                ]
              }
            ]
          }
          """

    val csv =
      """|en,cy
         |"Include the types of income generated in each country${if 1 = 1 then ', such as business profits or dividends.' else '. Acceptable income types are business profits or pensions.'}",CY1""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("translate sentence with new line 1") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "We will use these for your ongoing \ncorrespondence and returns for duty on gas for use as road fuel."
                    }
                  }
                ]
              }
            ]
          }"""

    val expected =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "We will use these for your ongoing \ncorrespondence and returns for duty on gas for use as road fuel.",
                      "cy": "CY1"
                    }
                  }
                ]
              }
            ]
          }
          """

    val csv =
      """|en,cy
         |We will use these for your ongoing \ncorrespondence and returns for duty on gas for use as road fuel.,CY1""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("translate sentence with new line 2") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "We will use these for your ongoing \ncorrespondence and returns for duty on gas for use as road fuel."
                    }
                  }
                ]
              }
            ]
          }"""

    val expected =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "We will use these for your ongoing \ncorrespondence and returns for duty on gas for use as road fuel.",
                      "cy": "CY1\nCY2"
                    }
                  }
                ]
              }
            ]
          }
          """

    val csv =
      """|en,cy
         |We will use these for your ongoing \ncorrespondence and returns for duty on gas for use as road fuel.,CY1\nCY2""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

  test("translate sentences with where one sentence is prefix of another") {
    val json =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "For each entity covered by this election, you’ll need to provide the:\n\n* name\n\n* address \n\n* name of qualifying company that owns or part owns the entity\n\n* percentage of entity owned by the qualifying company\n\n Include all the entities’ details in one document."
                    }
                  }
                ]
              }
            ]
          }"""

    val expected =
      json"""
          {
            "sections": [
              {
                "fields": [
                  {
                    "infoText": {
                      "en": "For each entity covered by this election, you’ll need to provide the:\n\n* name\n\n* address \n\n* name of qualifying company that owns or part owns the entity\n\n* percentage of entity owned by the qualifying company\n\n Include all the entities’ details in one document.",
                      "cy": "CY1\n\n* CY4\n\n* CY3 \n\n* CY5\n\n* CY6\n\n CY2"
                    }
                  }
                ]
              }
            ]
          }
          """

    val csv =
      """|en,cy
         |"For each entity covered by this election, you’ll need to provide the:",CY1
         |Include all the entities’ details in one document.,CY2
         |address,CY3
         |name,CY4
         |name of qualifying company that owns or part owns the entity,CY5
         |percentage of entity owned by the qualifying company,CY6""".stripMargin

    val translatableRows = TextExtractor.readCvsFromString(csv)
    val (res, stats) = TextExtractor.translateFile(translatableRows, json.spaces2)
    assertEquals(parse(res).toOption.get, expected)
  }

}
