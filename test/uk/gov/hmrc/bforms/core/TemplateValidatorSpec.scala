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

import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import org.scalatest.{ EitherValues, FlatSpec, Matchers }
import play.api.libs.json.{ JsNull, Json, JsNumber }
import uk.gov.hmrc.bforms.exceptions.InvalidState

class TemplateValidatorSpec extends FlatSpec with Matchers with EitherValues {

  "TemplateValidator.conform" should "validate template against template schema" in {

    val schema =
      """|{
         | "type": "object",
         | "properties": {
         |    "formTypeId": {
         |      "type": "string"
         |    },
         |    "sections": {
         |      "type": "array",
         |      "items": {
         |        "type": "object",
         |        "properties": {
         |          "title": {
         |            "type": "string"
         |          }
         |        },
         |        "required": [
         |          "title"
         |        ]
         |      }
         |    }
         |  },
         |  "required": [
         |    "formTypeId"
         |  ]
         |}""".stripMargin

    val template =
      """|{
         |  "formTypeId": "IPT100",
         |  "sections": [
         |    {
         |      "title": "Your details | eich manylion"
         |    }
         |  ]
         |}""".stripMargin

    val res =
      for {
        schemaRes <- SchemaValidator.conform(Json.parse(schema))
        tr <- TemplateValidator.conform(schemaRes, Json.parse(template)).toEither
      } yield tr

    res.right.value should be(())
  }

  it should "validate template with nested arrays against template schema" in {

    val schema =
      """|{
         | "type": "object",
         | "properties": {
         |    "formTypeId": {
         |      "type": "string"
         |    },
         |    "sections": {
         |      "type": "array",
         |      "items": {
         |        "type": "object",
         |        "properties": {
         |          "title": {
         |            "type": "string"
         |          },
         |          "fields": {
         |            "type": "array",
         |            "items": {
         |              "type": "object",
         |              "properties": {
         |                "id": {
         |                  "type": "string"
         |                },
         |                "label": {
         |                  "type": "string"
         |                },
         |                "mandatory": {
         |                  "type": "string"
         |                },
         |                "readonly": {
         |                  "type": "string"
         |                }
         |              },
         |              "required": [
         |                "id",
         |                "label"
         |              ]
         |            }
         |          }
         |        },
         |        "required": [
         |          "title"
         |        ]
         |      }
         |    }
         |  },
         |  "required": [
         |    "formTypeId"
         |  ]
         |}""".stripMargin

    val template =
      """|{
         |  "formTypeId": "IPT100",
         |  "sections": [
         |    {
         |      "title": "Your details | eich manylion",
         |      "fields": [
         |        {
         |          "id": "iptRegNum",
         |          "label": "Insurance Premium Tax (IPT) registration number | Treth Premiwm Yswiriant (IPT) rhif cofrestru",
         |          "readonly": "true",
         |          "mandatory": "true"
         |        }
         |      ]
         |    }
         |  ]
         |}""".stripMargin

    val res =
      for {
        schemaRes <- SchemaValidator.conform(Json.parse(schema))
        tr <- TemplateValidator.conform(schemaRes, Json.parse(template)).toEither
      } yield tr

    res.right.value should be(())
  }

  "TemplateValidator.extractFields" should "extract 'fields' objects from template json converted to TemplateField case class" in {

    val template =
      """|{
         |  "formTypeId": "IPT100",
         |  "sections": [
         |    {
         |      "title": "Your details | eich manylion",
         |      "fields": [
         |        {
         |          "id": "iptRegNum",
         |          "label": "Insurance Premium Tax (IPT) registration number | Treth Premiwm Yswiriant (IPT) rhif cofrestru",
         |          "readonly": "true",
         |          "mandatory": "true"
         |        }
         |      ]
         |    },
         |    {
         |      "title": "Rate for the period of the Insurance Premium Tax | Gyfradd ar gyfer y cyfnod y Dreth Premiwm Yswiriant",
         |      "fields": [
         |        {
         |          "id": "standardRateIPTDueForThisPeriod",
         |          "label": "Standard rate IPT due for this period | Cyfradd safonol IPT sy'n ddyledus am y cyfnod hwn",
         |          "format": "sterling",
         |          "mandatory": "true"
         |        },
         |        {
         |          "id": "higherRateIPTDueForThisPeriod",
         |          "label": "Higher rate IPT due for this period | Cyfradd uwch IPT sy'n ddyledus am y cyfnod hwn",
         |          "format": "sterling",
         |          "mandatory": "true"
         |        }
         |      ]
         |    }
         |  ]
         |}""".stripMargin

    val fieldsE = TemplateValidator.extractFields(Json.parse(template))

    fieldsE.right.value should be(
      List(
        TemplateField("iptRegNum", "true", None),
        TemplateField("standardRateIPTDueForThisPeriod", "true", Some("sterling")),
        TemplateField("higherRateIPTDueForThisPeriod", "true", Some("sterling"))
      )
    )
  }

  it should "extract nothing from empty json" in {

    val fieldsE = TemplateValidator.extractFields(JsNull)

    fieldsE.right.value should be(empty)
  }

  it should "return error when fieldName 'fields' is not an array" in {

    val template =
      """|{
         |  "fields": "some-string"
         |}""".stripMargin

    val fieldsE = TemplateValidator.extractFields(Json.parse(template))

    fieldsE.left.value should be(
      InvalidState("""|Error when reading 'TemplateField' class:
                      |Error: List((,List(ValidationError(List(error.expected.jsarray),WrappedArray()))))
                      |Input json: {
                      |  "fields" : "some-string"
                      |}""".stripMargin)
    )
  }

  it should "return error when fieldName 'fields' array contains no object not convertible to TemplateField" in {

    val template =
      """|{
         |  "fields": ["some-string"]
         |}""".stripMargin

    val fieldsE = TemplateValidator.extractFields(Json.parse(template))

    fieldsE.left.value should be(
      InvalidState("""|Error when reading 'TemplateField' class:
                      |Error: List(((0)/mandatory,List(ValidationError(List(error.path.missing),WrappedArray()))), ((0)/id,List(ValidationError(List(error.path.missing),WrappedArray()))))
                      |Input json: {
                      |  "fields" : [ "some-string" ]
                      |}""".stripMargin)
    )
  }
}
