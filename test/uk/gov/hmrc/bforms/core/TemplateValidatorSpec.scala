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
import uk.gov.hmrc.bforms.models.{ FieldValue, Schema, Section }

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
        schemaRes <- SchemaValidator.conform(Json.parse(schema).as[Schema])
        tr <- schemaRes.conform(Json.parse(template)).toEither
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
        schemaRes <- SchemaValidator.conform(Json.parse(schema).as[Schema])
        tr <- schemaRes.conform(Json.parse(template)).toEither
      } yield tr

    res.right.value should be(())
  }
}
