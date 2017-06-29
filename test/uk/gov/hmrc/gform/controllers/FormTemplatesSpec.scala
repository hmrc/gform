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

package uk.gov.hmrc.gform.controllers

import org.scalatest.time.{ Millis, Span }
import play.api.libs.json.{ JsObject, Json }
import uk.gov.hmrc.gform.core.ServiceResponse
import uk.gov.hmrc.gform.exceptions.InvalidState
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.typeclasses.{ FindOne, Update }
import uk.gov.hmrc.gform.{ FindOneCheck, Spec, TypeclassFixtures, UpdateCheck }

import scala.concurrent.ExecutionContext.Implicits._

class FormTemplatesSpec extends Spec with TypeclassFixtures {

  val plainFormTemplate = FormTemplate(Some("http://schemaId"), FormTypeId(""), "formName", Version("version"), "description", "characterSet", DmsSubmission("customerId", "classificationType", "businessArea"), "submitSuccessUrl", "submitErrorUrl", List.empty[Section])

  implicit override val patienceConfig = PatienceConfig(timeout = scaled(Span(500, Millis)), interval = scaled(Span(150, Millis)))

  "FormTemplates saveTemplate" should "return InvalidState when schema cannot be found" in {

    val findOneCheck = mock[FindOneCheck]

    implicit val findOne: FindOne[Schema] = FindOneTC
      .response(Option.empty[Schema]) // No schema will be found
      .callCheck(findOneCheck)
      .withChecks { req: JsObject =>
        req should be(Json.obj("id" -> "http://schemaId"))
      }

    implicit val update: Update[FormTemplate] = UpdateTC.notUsed[FormTemplate]

    (findOneCheck.call _).expects().once

    val res: ServiceResponse[DbOperationResult] = FormTemplates.saveTemplate(FormTypeId("abc"), Version("1.0.0"), plainFormTemplate)

    futureResult(res.value).left.value should be(InvalidState("SchemaId http://schemaId not found"))

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
              "properties" -> Json.obj("formName" -> Json.obj("type" -> "string")),
              "required" -> Json.arr()
            )
          )
        )
      )
      .callCheck(findOneCheck)
      .noChecks

    implicit val update: Update[FormTemplate] = UpdateTC
      .response(Right(UpdateSuccess))
      .callCheck(updateCheck)
      .withChecks { (selector: JsObject, formTemplate: FormTemplate) =>
        formTemplate should be(plainFormTemplate)
        selector should be(Json.obj("formTypeId" -> "abc", "version" -> "1.0.0"))
      }

    (findOneCheck.call _).expects().once
    (updateCheck.call _).expects().once

    val res: ServiceResponse[DbOperationResult] = FormTemplates.saveTemplate(FormTypeId("abc"), Version("1.0.0"), plainFormTemplate)

    futureResult(res.value).right.value should be(UpdateSuccess)
  }
}
