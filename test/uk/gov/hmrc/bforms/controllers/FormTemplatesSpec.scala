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

package uk.gov.hmrc.bforms.controllers

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Span }
import org.scalamock.scalatest.MockFactory
import play.api.libs.json.{ JsObject, Json }
import uk.gov.hmrc.bforms.{ FindOneCheck, TypeclassFixtures, UpdateCheck }
import uk.gov.hmrc.bforms.core.ServiceResponse
import uk.gov.hmrc.bforms.exceptions.InvalidState
import uk.gov.hmrc.bforms.models.{ FormTemplate, FormTypeId, DbOperationResult, Schema, UpdateSuccess }
import uk.gov.hmrc.bforms.typeclasses.{ FindOne, Update }

import scala.concurrent.ExecutionContext.Implicits._

class FormTemplatesSpec extends FlatSpec with Matchers with TypeclassFixtures with ScalaFutures with EitherValues with MockFactory {

  implicit override val patienceConfig = PatienceConfig(timeout = scaled(Span(500, Millis)), interval = scaled(Span(150, Millis)))

  "FormTemplates saveTemplate" should "return InvalidState when schema cannot be found" in {

    val findOneCheck = mock[FindOneCheck]

    implicit val findOne: FindOne[Schema] = FindOneTC
      .response(Option.empty[Schema]) // No schema will be found
      .callCheck(findOneCheck)
      .withChecks { req: JsObject =>
        req should be(Json.obj("id" -> "http://hmrc.gov.uk/jsonschema/bf-formtemplate#"))
      }

    implicit val update: Update[FormTemplate] = UpdateTC.notUsed[FormTemplate]

    (findOneCheck.call _).expects().once

    val res: ServiceResponse[DbOperationResult] = FormTemplates.saveTemplate(FormTypeId("abc"), "1.0.0", FormTemplate(Json.obj()))

    futureResult(res.value).left.value should be(InvalidState("SchemaId http://hmrc.gov.uk/jsonschema/bf-formtemplate# not found"))

  }

  it should "return UpdateSuccess when save was successful" in {
    val findOneCheck = mock[FindOneCheck]
    val updateCheck = mock[UpdateCheck]

    implicit val findOne: FindOne[Schema] = FindOneTC
      .response(
        Some(
          // Define schema FormTemplate must conform to
          Schema(
            Json.obj(
              "type" -> "object",
              "properties" -> Json.obj("firstName" -> Json.obj("type" -> "string")),
              "required" -> Json.arr()
            )
          )
        )
      )
      .callCheck(findOneCheck)
      .noChecks

    val formTemplateToSave = FormTemplate(Json.obj("firstName" -> "josef"))

    implicit val update: Update[FormTemplate] = UpdateTC
      .response(Right(UpdateSuccess))
      .callCheck(updateCheck)
      .withChecks { (selector: JsObject, formTemplate: FormTemplate) =>
        formTemplate should be(formTemplateToSave)
        selector should be(Json.obj("formTypeId" -> "abc", "version" -> "1.0.0"))
      }

    (findOneCheck.call _).expects().once
    (updateCheck.call _).expects().once

    val res: ServiceResponse[DbOperationResult] = FormTemplates.saveTemplate(FormTypeId("abc"), "1.0.0", formTemplateToSave)

    futureResult(res.value).right.value should be(UpdateSuccess)
  }
}
