/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.core

import cats.data.NonEmptyList
import cats.syntax.either._
import org.scalactic.source.Position
import org.scalatest.matchers.{ MatchResult, Matcher }
import org.scalatest.{ EitherValues, FlatSpec, Matchers }
import play.api.libs.json.{ Reads, _ }
import uk.gov.hmrc.bforms.exceptions.InvalidState
import uk.gov.hmrc.bforms.models.{ FieldId, FieldValue, FormField, Schema, Section }

class FieldValueSpec extends FlatSpec with Matchers with EitherValues with JsResultMatcher {

  "FieldValue json object" should "parse as Text if it not include 'type' field" in {
    val fieldValue = toFieldValue(
      """|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text, "Registration number", None, None, None, None, true, None))
  }

  it should "parse 'choice' type as Radio with Vertical orientation if no multivalue & no format is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ]
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical),
        "Select the tax type",
        None,
        None,
        None,
        None,
        true,
        None
      )
    )
  }

  it should "parse 'choice' type as Radio with Vertical orientation if 'multivalue=no' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"no"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical),
        "Select the tax type",
        None,
        None,
        None,
        None,
        true,
        None
      )
    )
  }

  it should "parse 'choice' type as Radio with Vertical orientation if 'multivalue=no & format=vertical' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"no",
         |  "format":"vertical"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical),
        "Select the tax type",
        None,
        Some(TextExpression("vertical")),
        None,
        None,
        true,
        None
      )
    )
  }

  it should "parse 'choice' type as Radio with Horizontal orientation if 'multivalue=no & format=horizontal' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"no",
         |  "format":"horizontal"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Horizontal),
        "Select the tax type",
        None,
        Some(TextExpression("horizontal")),
        None,
        None,
        true,
        None
      )
    )
  }

  it should "parse 'choice' type as Checkbox with Vertical orientation if 'multivalue=yes' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"yes"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Vertical),
        "Select the tax type",
        None,
        None,
        None,
        None,
        true,
        None
      )
    )
  }

  it should "parse 'choice' type as Checkbox with Vertical orientation if 'multivalue=yes & format=vertical' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"yes",
         |  "format":"vertical"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Vertical),
        "Select the tax type",
        None,
        Some(TextExpression("vertical")),
        None,
        None,
        true,
        None
      )
    )
  }

  it should "parse 'choice' type as Checkbox with Horizontal orientation if 'multivalue=yes & format=horizontal' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"yes",
         |  "format":"horizontal"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Horizontal),
        "Select the tax type",
        None,
        Some(TextExpression("horizontal")),
        None,
        None,
        true,
        None
      )
    )
  }

  it should "parse 'choice' type as YesNo if 'format=yesno' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal),
        "Gas tax type?",
        None,
        Some(TextExpression("yesno")),
        None,
        None,
        true,
        None
      )
    )
  }

  it should "parse 'choice' type as YesNo if 'format=yesno & multivalue=no' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "multivalue":"no"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal),
        "Gas tax type?",
        None,
        Some(TextExpression("yesno")),
        None,
        None,
        true,
        None
      )
    )
  }

  it should "fail to parse 'choice' type as YesNo if 'choices' are provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ]
         |}"""
    )

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type as YesNo if 'multivalue=yes' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "multivalue":"yes"
         |}"""
    )

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type if no 'options' are provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type"
         |}"""
    )

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type if 'multivalue' is not 'yes' or 'no'" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"wrong-value"
         |}"""
    )

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type if 'format' is not 'vertical' or 'horizontal' or 'yesno'" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "format":"wrong-value"
         |}"""
    )

    fieldValue should be(jsError)
  }

  private def toFieldValue(template: String): JsResult[FieldValue] = {

    val templateAsJson = Json.parse(template.stripMargin)

    implicitly[Reads[FieldValue]].reads(templateAsJson)
  }
}
