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

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OverseasAddress.Configurable._

class FormComponentMakerSpec extends AnyFlatSpecLike with Matchers with FormTemplateSupport {

  "optMaybeFormatExpr" should "parse and validate format expression" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "format": "text"
                                                                 |}
                                                                 |""".stripMargin))

    val result: Opt[Option[FormatExpr]] =
      formComponentMaker.optMaybeFormatExpr(RoundingMode.defaultRoundingMode)(None)(EmailVerification.noVerification)
    result shouldBe Right(Some(TextFormat(TextWithRestrictions(0, 1000))))
  }

  it should "return error when format is not valid" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "format": "some-invalid-format"
                                                                 |}
                                                                 |""".stripMargin))

    val result: Opt[Option[FormatExpr]] =
      formComponentMaker.optMaybeFormatExpr(RoundingMode.defaultRoundingMode)(None)(EmailVerification.noVerification)
    result shouldBe Left(UnexpectedState("""|Unable to parse expression some-invalid-format.
                                            |Errors:
                                            |end of input expected""".stripMargin))
  }

  "textOpt" should "parse text component" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(Text(TextWithRestrictions(0, 1000), Value))
  }

  it should "parse text component with both prefix and suffix" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "prefix": "prefixTest",
                                                                 |   "suffix": "suffixTest"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(
      Text(
        TextWithRestrictions(0, 1000),
        Value,
        prefix = Some(toSmartString("prefixTest")),
        suffix = Some(toSmartString("suffixTest"))
      )
    )
  }

  it should "parse text component with only prefix" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "prefix": "prefixTest"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(Text(TextWithRestrictions(0, 1000), Value, prefix = Some(toSmartString("prefixTest"))))
  }

  it should "parse text component with only suffix" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "suffix": "suffixTest"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(Text(TextWithRestrictions(0, 1000), Value, suffix = Some(toSmartString("suffixTest"))))
  }

  it should "return error when format is not valid for text" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "invalid"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Left(UnexpectedState("""|Missing or invalid format for text field
                                            |Id: id1
                                            |Format: Some(OrientationFormat(invalid))
                                            |Value: None
                                            |""".stripMargin))
  }

  it should "parse multiline text component" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": true
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value, dataThreshold = None))
  }

  it should "parse multiline text component with no of rows" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": true,
                                                                 |   "rows": 7
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value, rows = 7, dataThreshold = None))
  }

  it should "parse multiline text component with displayCharCount false" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": true,
                                                                 |   "displayCharCount": false
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(
      TextArea(TextWithRestrictions(0, 1000), Value, displayCharCount = false, dataThreshold = None)
    )
  }

  it should "parse multiline text component with displayCharCount true" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": true,
                                                                 |   "displayCharCount": true
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value, dataThreshold = None))
  }

  it should "parse multiline text component without displayCharCount" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": true
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value, dataThreshold = None))
  }

  it should "return error when format is not valid for multiline text" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "invalid",
                                                                 |   "multiline": true
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Left(UnexpectedState("""|Missing or invalid format for multiline text field
                                            |Id: id1
                                            |Format: Some(OrientationFormat(invalid))
                                            |Value: None
                                            |""".stripMargin))
  }

  it should "parse calendarDate component" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "calendarDate1",
                                                                 |   "type": "calendarDate",
                                                                 |   "label": "calendarDate1"
                                                                 |}
                                                                 |""".stripMargin))
    formComponentMaker.optFieldValue() shouldBe Right(mkFormComponent("calendarDate1", CalendarDate, true))
  }

  it should "parse calendarDate component with labelSize" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "calendarDate1",
                                                                 |   "type": "calendarDate",
                                                                 |   "label": "calendarDate1",
                                                                 |   "labelSize": "m"
                                                                 |}
                                                                 |""".stripMargin))
    formComponentMaker.optFieldValue() shouldBe Right(
      mkFormComponentWithLabelSize("calendarDate1", CalendarDate, Some(Medium))
    )
  }

  it should "return error when labelSize input is not valid" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "calendarDate1",
                                                                 |   "type": "calendarDate",
                                                                 |   "label": "calendarDate1",
                                                                 |   "labelSize": "invalid"
                                                                 |}
                                                                 |""".stripMargin))
    formComponentMaker.optFieldValue() shouldBe Left(
      UnexpectedState("""|Unable to parse expression invalid.
                         |Errors:
                         |'xs' expected but 'i' found""".stripMargin)
    )
  }

  it should "parse summaryList" in {
    val summaryListJson = Json.parse("""
                                       |{
                                       |          "id": "bankDetailsConfirmedInfo",
                                       |          "type": "miniSummaryList",
                                       |          "keyDisplayWidth": "m",
                                       |          "label": "summaryListLabel",
                                       |          "rows": [
                                       |            {
                                       |              "includeIf": "${2 = 3}",
                                       |              "key": "Sort Code",
                                       |              "value": "sortCode"
                                       |            },
                                       |            {
                                       |              "header": "test header"
                                       |            },
                                       |            {
                                       |              "includeIf": "${15 = 11}",
                                       |              "value": "${2 + 3}"
                                       |            },
                                       |            {
                                       |              "includeIf": "${11 = 11}",
                                       |              "value": "${2 + 3}",
                                       |              "pageId": "atlId"
                                       |            },
                                       |            {
                                       |              "atlId": "atlID",
                                       |              "repeat": [
                                       |                {
                                       |                  "header": "test repeat header"
                                       |                }
                                       |              ]
                                       |            },
                                       |            {
                                       |              "key": "Number + Date",
                                       |              "value": "${numericAmount} - ${enteredDate}",
                                       |              "includeIf": "${1 = 1}"
                                       |            }
                                       |          ]
                                       |    }
                                       |""".stripMargin)
    val formComponentMaker = new FormComponentMaker(summaryListJson)
    val result = formComponentMaker.optFieldValue()

    result shouldBe Right(
      FormComponent(
        FormComponentId("bankDetailsConfirmedInfo"),
        MiniSummaryList(
          List(
            MiniSummaryRow.ValueRow(
              Some(SmartString(LocalisedString(Map(LangADT.En -> "Sort Code")), List())),
              MiniSummaryListValue.Reference(FormCtx(FormComponentId("sortCode"))),
              Some(IncludeIf(Equals(Constant("2"), Constant("3")))),
              None
            ),
            MiniSummaryRow.HeaderRow(
              SmartString(LocalisedString(Map(LangADT.En -> "test header")), List())
            ),
            MiniSummaryRow.ValueRow(
              None,
              MiniSummaryListValue.AnyExpr(Add(Constant("2"), Constant("3"))),
              Some(IncludeIf(Equals(Constant("15"), Constant("11")))),
              None
            ),
            MiniSummaryRow.ValueRow(
              None,
              MiniSummaryListValue.AnyExpr(Add(Constant("2"), Constant("3"))),
              Some(IncludeIf(Equals(Constant("11"), Constant("11")))),
              Some(PageId("atlId"))
            ),
            MiniSummaryRow.ATLRow(
              FormComponentId("atlID"),
              None,
              List(
                MiniSummaryRow.HeaderRow(
                  SmartString(LocalisedString(Map(LangADT.En -> "test repeat header")), List())
                )
              )
            ),
            MiniSummaryRow.SmartStringRow(
              Some(SmartString(LocalisedString(Map(LangADT.En -> "Number + Date")), List())),
              SmartString(
                LocalisedString(Map(LangADT.En -> "{0} - {1}")),
                List(FormCtx(FormComponentId("numericAmount")), FormCtx(FormComponentId("enteredDate")))
              ),
              Some(IncludeIf(Equals(Constant("1"), Constant("1")))),
              None
            )
          ),
          Some(KeyDisplayWidth.M)
        ),
        SmartString(LocalisedString(Map(LangADT.En -> "summaryListLabel")), List()),
        false,
        None,
        None,
        None,
        None,
        true,
        true,
        true,
        false,
        false,
        None,
        None,
        List(),
        None,
        None
      )
    )
  }

  it should "parse table" in {
    val tableJson = Json.parse("""
                                 | {
                                 |   "id": "gformTable",
                                 |   "type": "table",
                                 |   "label": "Table of data",
                                 |   "summaryValue": "Summary value",
                                 |   "caption": "This is caption",
                                 |   "captionClasses": "test-caption-classes",
                                 |   "firstCellIsHeader": "false",
                                 |   "classes": "test-classes",
                                 |   "header": [
                                 |     {
                                 |        "label": "Column 1"
                                 |     },
                                 |     {
                                 |        "label": "Column 2"
                                 |     },
                                 |     {
                                 |        "label": "Column 3"
                                 |     }
                                 |   ],
                                 |   "rows": [
                                 |     {
                                 |       "includeIf": "${expression}",
                                 |       "values": [
                                 |         {
                                 |           "class": "header",
                                 |           "value": "Row 1"
                                 |         },
                                 |         {
                                 |           "value": "${value1}"
                                 |         },
                                 |         {
                                 |           "value": "${value2}"
                                 |         }
                                 |       ]
                                 |     },
                                 |     {
                                 |       "includeIf": "${expression}",
                                 |       "values": [
                                 |         {
                                 |           "class": "header",
                                 |           "value": "Row 2"
                                 |         },
                                 |         {
                                 |           "colspan": 2,
                                 |           "value": "${value3}"
                                 |         }
                                 |       ]
                                 |     },
                                 |     {
                                 |       "values": [
                                 |         {
                                 |           "class": "header-xl",
                                 |           "value": "Total due"
                                 |         },
                                 |         {
                                 |           "class": "header-xxl",
                                 |           "value": "${total1}"
                                 |         },
                                 |         {
                                 |           "class": "header-xxl",
                                 |           "value": "${total2}"
                                 |         }
                                 |       ]
                                 |     }
                                 |   ]
                                 | }
                                 |""".stripMargin)
    val tableMaker = new FormComponentMaker(tableJson)
    val result = tableMaker.optFieldValue()
    result shouldBe Right(
      FormComponent(
        FormComponentId("gformTable"),
        TableComp(
          List(
            TableHeaderCell(SmartString(LocalisedString(Map(LangADT.En -> "Column 1")), List()), None),
            TableHeaderCell(SmartString(LocalisedString(Map(LangADT.En -> "Column 2")), List()), None),
            TableHeaderCell(SmartString(LocalisedString(Map(LangADT.En -> "Column 3")), List()), None)
          ),
          List(
            TableValueRow(
              List(
                TableValue(
                  SmartString(LocalisedString(Map(LangADT.En -> "Row 1")), List()),
                  Some("header"),
                  None,
                  None
                ),
                TableValue(
                  SmartString(
                    LocalisedString(Map(LangADT.En -> "{0}")),
                    List(FormCtx(FormComponentId("value1")))
                  ),
                  None,
                  None,
                  None
                ),
                TableValue(
                  SmartString(
                    LocalisedString(Map(LangADT.En -> "{0}")),
                    List(FormCtx(FormComponentId("value2")))
                  ),
                  None,
                  None,
                  None
                )
              ),
              Some(IncludeIf(TopLevelRef(BooleanExprId("expression")))),
              Option.empty[Dynamic]
            ),
            TableValueRow(
              List(
                TableValue(
                  SmartString(LocalisedString(Map(LangADT.En -> "Row 2")), List()),
                  Some("header"),
                  None,
                  None
                ),
                TableValue(
                  SmartString(
                    LocalisedString(Map(LangADT.En -> "{0}")),
                    List(FormCtx(FormComponentId("value3")))
                  ),
                  None,
                  Some(2),
                  None
                )
              ),
              Some(IncludeIf(TopLevelRef(BooleanExprId("expression")))),
              Option.empty[Dynamic]
            ),
            TableValueRow(
              List(
                TableValue(
                  SmartString(LocalisedString(Map(LangADT.En -> "Total due")), List()),
                  Some("header-xl"),
                  None,
                  None
                ),
                TableValue(
                  SmartString(
                    LocalisedString(Map(LangADT.En -> "{0}")),
                    List(FormCtx(FormComponentId("total1")))
                  ),
                  Some("header-xxl"),
                  None,
                  None
                ),
                TableValue(
                  SmartString(
                    LocalisedString(Map(LangADT.En -> "{0}")),
                    List(FormCtx(FormComponentId("total2")))
                  ),
                  Some("header-xxl"),
                  None,
                  None
                )
              ),
              None,
              Option.empty[Dynamic]
            )
          ),
          toSmartString("Summary value"),
          Some("This is caption"),
          "test-caption-classes",
          "test-classes",
          false
        ),
        SmartString(LocalisedString(Map(LangADT.En -> "Table of data")), List()),
        false,
        None,
        None,
        None,
        None,
        true,
        true,
        true,
        false,
        false,
        None,
        None,
        List(),
        None,
        None
      )
    )
  }

  it should "parse Address with FormCtx value" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |  "id": "userAddressCopy",
                                                                 |  "type": "address",
                                                                 |  "label": "Address copy",
                                                                 |  "submitMode": "summaryinfoonly",
                                                                 |  "value": "${userAddress}"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.optFieldValue().map(_.`type`)
    result shouldBe Right(Address(false, List(), false, Some(FormCtx(FormComponentId("userAddress")))))
  }

  it should "parse Address with auth.itmpAddress value" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |  "id": "userAddressCopy",
                                                                 |  "type": "address",
                                                                 |  "label": "Address copy",
                                                                 |  "submitMode": "summaryinfoonly",
                                                                 |  "value": "${auth.itmpAddress}"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.optFieldValue().map(_.`type`)
    result shouldBe Right(Address(false, List(), false, Some(AuthCtx(AuthInfo.ItmpAddress))))
  }

  it should "parse Overseas Address with FormCtx value" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |  "type": "overseasAddress",
                                                                 |  "id": "colombiaAddress",
                                                                 |  "label": "",
                                                                 |  "line2Mandatory": true,
                                                                 |  "cityMandatory": false,
                                                                 |  "postcodeMandatory": true,
                                                                 |  "value": "${userAddress}"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.optFieldValue().map(_.`type`)
    val expected = OverseasAddress(
      List(Mandatory.Line2, Mandatory.Postcode),
      List(Optional.City),
      true,
      Some(FormCtx(FormComponentId("userAddress"))),
      true
    )
    result shouldBe Right(expected)
  }

  it should "parse Overseas Address with itmpAddress value" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |  "type": "overseasAddress",
                                                                 |  "id": "colombiaAddress",
                                                                 |  "label": "",
                                                                 |  "line2Mandatory": true,
                                                                 |  "cityMandatory": false,
                                                                 |  "postcodeMandatory": true,
                                                                 |  "value": "${auth.itmpAddress}"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.optFieldValue().map(_.`type`)
    val expected = OverseasAddress(
      List(Mandatory.Line2, Mandatory.Postcode),
      List(Optional.City),
      true,
      Some(AuthCtx(AuthInfo.ItmpAddress)),
      true
    )
    result shouldBe Right(expected)
  }

  it should "parse text component with errorShortName" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "errorShortName": "error short name"
                                                                 |}
                                                                 |""".stripMargin))
    formComponentMaker.optFieldValue().map(_.errorShortName) shouldBe Right(Some(toSmartString("error short name")))
  }

  it should "parse text component with errorShortNameStart" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "errorShortNameStart": "error short name start"
                                                                 |}
                                                                 |""".stripMargin))
    formComponentMaker.optFieldValue().map(_.errorShortNameStart) shouldBe Right(
      Some(toSmartString("error short name start"))
    )
  }

  it should "parse text component with errorExample" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "errorExample": "error example"
                                                                 |}
                                                                 |""".stripMargin))
    formComponentMaker.optFieldValue().map(_.errorExample) shouldBe Right(
      Some(toSmartString("error example"))
    )
  }

}
