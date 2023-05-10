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

package uk.gov.hmrc.gform.builder

import cats.implicits._
import io.circe._
import io.circe.CursorOp._
import io.circe.Json
import play.api.libs.circe.Circe
import play.api.libs.json.JsObject
import play.api.libs.json.{ Json => PlayJson }
import play.api.mvc.{ ControllerComponents, Result, Results }
import scala.concurrent.Future
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, FormTemplatesControllerRequestHandler, RequestHandlerAlg }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.concurrent.ExecutionContext

sealed trait BuilderError extends Product with Serializable

object BuilderError {
  final case class CirceError(failure: ParsingFailure) extends BuilderError
  final case class PlayJsonError(message: String) extends BuilderError
}

object BuilderSupport {

  private def modifyJson(
    formTemplateRaw: FormTemplateRaw
  )(updateFunction: Json => Json): Either[BuilderError, FormTemplateRaw] = {
    // Converting Play Json => Circe Json => Play Json is suboptimal, let's consider
    // moving solely to Circe Json
    val jsonString: String = PlayJson.stringify(formTemplateRaw.value)
    val maybeCirceJson: Either[BuilderError, Json] =
      io.circe.parser.parse(jsonString).leftMap(BuilderError.CirceError(_))

    maybeCirceJson.flatMap { json =>
      val result: Json = updateFunction(json)
      PlayJson.parse(result.noSpaces) match {
        case json: JsObject => Right(FormTemplateRaw(json))
        case unexpected     => Left(BuilderError.PlayJsonError(s"Expected json object got: $unexpected"))
      }
    }
  }

  def modifySectionData(json: Json, sectionNumber: Int, sectionData: Json): Json =
    List("title", "caption", "description", "shortName", "continueLabel", "presentationHint").foldRight(json) {
      case (property, accJson) =>
        sectionData.hcursor
          .downField(property)
          .focus
          .flatMap { propertyValue =>
            val sectionPath: List[CursorOp] = (0 until sectionNumber).foldRight(List[CursorOp](DownArray)) {
              case (_, acc) =>
                MoveRight :: acc
            }

            val history: List[CursorOp] = sectionPath ::: DownField("sections") :: Nil

            val array = accJson.hcursor.replay(history)

            array.success.flatMap { hcursor =>
              hcursor
                .downField(property)
                .set(propertyValue)
                .root
                .focus
            }
          }
          .getOrElse(accJson)

    }

  def modifyFormComponentData(
    json: Json,
    sectionNumber: Int,
    formComponentId: FormComponentId,
    sectionData: Json
  ): Json =
    List("label", "helpText").foldRight(json) { case (property, accJson) =>
      sectionData.hcursor
        .downField(property)
        .focus
        .flatMap { propertyValue =>
          val sectionPath: List[CursorOp] = (0 until sectionNumber).foldRight(List[CursorOp](DownArray)) {
            case (_, acc) =>
              MoveRight :: acc
          }

          val history: List[CursorOp] =
            DownArray :: DownField("fields") :: sectionPath ::: DownField("sections") :: Nil

          val array = accJson.hcursor.replay(history)

          array.success.flatMap { hcursor =>
            hcursor
              .find { json =>
                json.hcursor.downField("id").focus.flatMap(_.asString).contains(formComponentId.value)
              }
              .downField(property)
              .set(propertyValue)
              .root
              .focus
          }
        }
        .getOrElse(accJson)
    }

  def updateFormComponent(
    formTemplateRaw: FormTemplateRaw,
    sectionNumber: Int,
    sectionData: Json,
    formComponentId: FormComponentId
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifyFormComponentData(_, sectionNumber, formComponentId, sectionData))

  def updateSectionByIndex(
    formTemplateRaw: FormTemplateRaw,
    sectionNumber: Int,
    sectionData: Json
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifySectionData(_, sectionNumber, sectionData))
}

class BuilderController(controllerComponents: ControllerComponents, formTemplateService: FormTemplateService)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) with Circe {

  private val requestHandler: RequestHandlerAlg[FOpt] =
    new FormTemplatesControllerRequestHandler(
      formTemplateService.verifyAndSave,
      formTemplateService.save
    ).futureInterpreter

  private def applyUpdateFunction(
    formTemplateRawId: FormTemplateRawId
  )(
    updateFormTemplateRaw: FormTemplateRaw => Either[BuilderError, (FormTemplateRaw, Result)]
  ) = {

    val jsResult: Future[Either[BuilderError, (FormTemplateRaw, Result)]] = for {
      formTemplateRaw <- formTemplateService.get(formTemplateRawId)
    } yield updateFormTemplateRaw(formTemplateRaw)

    jsResult.flatMap {
      case Right((templateRaw, result)) =>
        requestHandler
          .handleRequest(templateRaw)
          .fold(
            _.asBadRequest,
            _ => result
          )
      case Left(error) => BadRequest(error.toString).pure[Future]
    }
  }

  private def updateAction(formTemplateRawId: FormTemplateRawId)(
    updateFunction: (FormTemplateRaw, Json) => Either[BuilderError, (FormTemplateRaw, Result)]
  ) =
    Action.async(circe.json) { implicit request =>
      applyUpdateFunction(formTemplateRawId)(formTemplateRaw => updateFunction(formTemplateRaw, request.body))
    }

  def updateSectionByIndex(formTemplateRawId: FormTemplateRawId, sectionNumber: Int) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .updateSectionByIndex(formTemplateRaw, sectionNumber, requestBody)
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }

  def updateFormComponent(formTemplateRawId: FormTemplateRawId, sectionNumber: Int, formComponentId: FormComponentId) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .updateFormComponent(formTemplateRaw, sectionNumber, requestBody, formComponentId)
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }
}
