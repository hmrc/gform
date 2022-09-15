/*
 * Copyright 2022 HM Revenue & Customs
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
                                                                 |   "multiline": "yes"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value))
  }

  it should "parse multiline text component with no of rows" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": "yes",
                                                                 |   "rows": 7
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value, rows = 7))
  }

  it should "parse multiline text component with displayCharCount false" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": "yes",
                                                                 |   "displayCharCount": "false"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value, displayCharCount = false))
  }

  it should "parse multiline text component with displayCharCount no" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": "yes",
                                                                 |   "displayCharCount": "no"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value, displayCharCount = false))
  }

  it should "parse multiline text component with displayCharCount nO" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": "yes",
                                                                 |   "displayCharCount": "false"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value, displayCharCount = false))
  }

  it should "parse multiline text component with displayCharCount fAlSe" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": "yes",
                                                                 |   "displayCharCount": "fAlSe"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value, displayCharCount = false))
  }

  it should "parse multiline text component with displayCharCount true" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": "yes",
                                                                 |   "displayCharCount": "true"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value))
  }

  it should "parse multiline text component with displayCharCount tRUe" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": "yes",
                                                                 |   "displayCharCount": "tRUe"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value))
  }

  it should "parse multiline text component with displayCharCount noTrurOrFalse" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": "yes",
                                                                 |   "displayCharCount": "noTrurOrFalse"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value))
  }

  it should "parse multiline text component without displayCharCount" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "text",
                                                                 |   "multiline": "yes"
                                                                 |}
                                                                 |""".stripMargin))
    val result = formComponentMaker.textOpt
    result shouldBe Right(TextArea(TextWithRestrictions(0, 1000), Value))
  }

  it should "return error when format is not valid for multiline text" in {
    val formComponentMaker = new FormComponentMaker(Json.parse("""
                                                                 |{
                                                                 |   "id": "id1",
                                                                 |   "type": "text",
                                                                 |   "label": "Field 1",
                                                                 |   "format": "invalid",
                                                                 |   "multiline": "yes"
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
                                       |              "atlId": "atlID",
                                       |              "repeat": [
                                       |                {
                                       |                  "header": "test repeat header" 
                                       |                }
                                       |              ]
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
              Some(IncludeIf(Equals(Constant("2"), Constant("3"))))
            ),
            MiniSummaryRow.HeaderRow(
              SmartString(LocalisedString(Map(LangADT.En -> "test header")), List())
            ),
            MiniSummaryRow.ValueRow(
              None,
              MiniSummaryListValue.AnyExpr(Add(Constant("2"), Constant("3"))),
              Some(IncludeIf(Equals(Constant("15"), Constant("11"))))
            ),
            MiniSummaryRow.ATLRow(
              FormComponentId("atlID"),
              None,
              List(
                MiniSummaryRow.HeaderRow(
                  SmartString(LocalisedString(Map(LangADT.En -> "test repeat header")), List())
                )
              )
            )
          )
        ),
        SmartString(LocalisedString(Map(LangADT.En -> "summaryListLabel")), List()),
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
                                 |  "caption": "This is caption",
                                 |  "captionClasses": "test-caption-classes",
                                 |  "firstCellIsHeader": "false",
                                 |  "classes": "test-classes",
                                 |   "header": [
                                 |     "Column 1",
                                 |     "Column 2",
                                 |     "Column 3"
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
            SmartString(LocalisedString(Map(LangADT.En -> "Column 1")), List()),
            SmartString(LocalisedString(Map(LangADT.En -> "Column 2")), List()),
            SmartString(LocalisedString(Map(LangADT.En -> "Column 3")), List())
          ),
          List(
            TableValueRow(
              List(
                TableValue(
                  SmartString(LocalisedString(Map(LangADT.En -> "Row 1")), List()),
                  Some("header"),
                  None
                ),
                TableValue(
                  SmartString(
                    LocalisedString(Map(LangADT.En -> "{0}")),
                    List(FormCtx(FormComponentId("value1")))
                  ),
                  None,
                  None
                ),
                TableValue(
                  SmartString(
                    LocalisedString(Map(LangADT.En -> "{0}")),
                    List(FormCtx(FormComponentId("value2")))
                  ),
                  None,
                  None
                )
              ),
              Some(IncludeIf(TopLevelRef(BooleanExprId("expression"))))
            ),
            TableValueRow(
              List(
                TableValue(
                  SmartString(LocalisedString(Map(LangADT.En -> "Row 2")), List()),
                  Some("header"),
                  None
                ),
                TableValue(
                  SmartString(
                    LocalisedString(Map(LangADT.En -> "{0}")),
                    List(FormCtx(FormComponentId("value3")))
                  ),
                  None,
                  Some(2)
                )
              ),
              Some(IncludeIf(TopLevelRef(BooleanExprId("expression"))))
            ),
            TableValueRow(
              List(
                TableValue(
                  SmartString(LocalisedString(Map(LangADT.En -> "Total due")), List()),
                  Some("header-xl"),
                  None
                ),
                TableValue(
                  SmartString(
                    LocalisedString(Map(LangADT.En -> "{0}")),
                    List(FormCtx(FormComponentId("total1")))
                  ),
                  Some("header-xxl"),
                  None
                ),
                TableValue(
                  SmartString(
                    LocalisedString(Map(LangADT.En -> "{0}")),
                    List(FormCtx(FormComponentId("total2")))
                  ),
                  Some("header-xxl"),
                  None
                )
              ),
              None
            )
          ),
          Some("This is caption"),
          "test-caption-classes",
          "test-classes",
          false
        ),
        SmartString(LocalisedString(Map(LangADT.En -> "Table of data")), List()),
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
}
