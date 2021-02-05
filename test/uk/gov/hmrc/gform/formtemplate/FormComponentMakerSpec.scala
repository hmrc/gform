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

package uk.gov.hmrc.gform.formtemplate

import org.scalatest.{ FlatSpecLike, Matchers }
import play.api.libs.json.Json
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormatExpr, RoundingMode, Text, TextArea, TextFormat, TextWithRestrictions, Value }

class FormComponentMakerSpec extends FlatSpecLike with Matchers {

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
                                            |some-invalid-format:1: unexpected trailing characters
                                            |some-invalid-format    ^""".stripMargin))
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
        suffix = Some(toSmartString("suffixTest"))))
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

}
