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

package uk.gov.hmrc.gform.core

import org.scalatest.{ EitherValues, FlatSpec, Matchers }
import play.api.libs.json.{ JsNull, Json, JsNumber }
import uk.gov.hmrc.gform.exceptions.{ InvalidState, InvalidStateWithJson }
import uk.gov.hmrc.gform.models.Schema

class SchemaValidatorSpec extends FlatSpec with Matchers with EitherValues {

  "SchemaValidator" should "read SSobject" in {

    val input =
      """|{
         |  "type": "object",
         |  "properties": {
         |    "formTypeId": {
         |      "type": "string"
         |    }
         |  },
         |  "required": [
         |    "formTypeId"
         |  ]
         |}""".stripMargin

    val res = SchemaValidator.conform(Json.parse(input).as[Schema])
    res.right.value should be(SObject(List(Item("formTypeId", SString)), List("formTypeId")))
  }

  it should "fail for json not containing 'type' fieldName" in {
    val res = SchemaValidator.conform(Schema(Json.obj()))
    res.left.value should be(InvalidStateWithJson("No 'type' fieldName found in json", Json.obj()))
  }

  it should "fail when 'type' fieldName is not a String" in {
    val res = SchemaValidator.conform(Schema(Json.obj("type" -> 1)))
    res.left.value should be(InvalidStateWithJson("Expected 'type' to be one of 4 strings in json", Json.obj("type" -> 1)))
  }

  it should "read list of SSobject" in {
    val input =
      """|{
         | "type": "object",
         | "properties": {
         |    "formTypeId": {
         |      "type": "string"
         |    },
         |    "archived": {
         |      "type": "boolean"
         |    }
         |  },
         |  "required": [
         |    "formTypeId"
         |  ]
         |}""".stripMargin

    val res = SchemaValidator.conform(Json.parse(input).as[Schema])
    res.right.value should be(SObject(List(Item("formTypeId", SString), Item("archived", SBoolean)), List("formTypeId")))
  }

  it should "read example schema" in {
    val input =
      """|{
         |  "id": "http://hmrc.gov.uk/jsonschema/bf-formtemplate#",
         |  "$schema": "http://json-schema.org/draft-04/schema#",
         |  "description": "Schema for a BetterForms Form Template",
         |  "type": "object",
         |  "properties": {
         |     "formTypeId": {
         |       "type": "string"
         |     },
         |     "formName": {
         |       "type": "string"
         |     },
         |     "version": {
         |       "type": "string"
         |     },
         |     "description": {
         |       "type": "string"
         |     },
         |     "archived": {
         |       "type": "boolean"
         |     },
         |     "characterSet": {
         |       "type": "string"
         |     },
         |     "submitTargets": {
         |       "type": "string"
         |     },
         |     "submitSuccessUrl": {
         |       "type": "string"
         |     },
         |     "submitErrorUrl": {
         |       "type": "string"
         |     },
         |     "sections": {
         |       "type": "array",
         |       "items": {
         |         "type": "object",
         |         "properties": {
         |           "title": {
         |             "type": "string"
         |           },
         |           "fields": {
         |             "type": "array",
         |             "items": {
         |               "type": "object",
         |               "properties": {
         |                 "id": {
         |                   "type": "string"
         |                 },
         |                 "label": {
         |                   "type": "string"
         |                 },
         |                 "value": {
         |                   "type": "string"
         |                 },
         |                 "format": {
         |                   "type": "string"
         |                 },
         |                 "helpText": {
         |                   "type": "string"
         |                 },
         |                 "readonly": {
         |                   "type": "string"
         |                 }
         |               },
         |               "required": [
         |                 "id",
         |                 "label"
         |               ]
         |             }
         |           }
         |         },
         |         "required": [
         |           "title",
         |           "fields"
         |         ]
         |       }
         |     }
         |   },
         |   "required": [
         |     "formTypeId",
         |     "formName",
         |     "version",
         |     "sections"
         |   ]
         |}""".stripMargin

    val res = SchemaValidator.conform(Json.parse(input).as[Schema])

    res.right.value should be(
      SObject(
        List(
          Item("formTypeId", SString),
          Item("formName", SString),
          Item("version", SString),
          Item("description", SString),
          Item("archived", SBoolean),
          Item("characterSet", SString),
          Item("submitTargets", SString),
          Item("submitSuccessUrl", SString),
          Item("submitErrorUrl", SString),
          Item(
            "sections",
            SArray(
              SObject(
                List(
                  Item("title", SString),
                  Item(
                    "fields",
                    SArray(
                      SObject(
                        List(
                          Item("id", SString),
                          Item("label", SString),
                          Item("value", SString),
                          Item("format", SString),
                          Item("helpText", SString),
                          Item("readonly", SString)
                        ),
                        List("id", "label")
                      )
                    )
                  )
                ),
                List("title", "fields")
              )
            )
          )
        ),
        List("formTypeId", "formName", "version", "sections")
      )
    )
  }
}
