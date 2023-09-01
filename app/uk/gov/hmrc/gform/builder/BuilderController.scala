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
import scala.util.matching.Regex
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

sealed trait PropertyBehaviour extends Product with Serializable

object PropertyBehaviour {
  // Set property to whatever value has been received from the client
  case object Normal extends PropertyBehaviour
  // Remove property if received value from client is empty
  case object PurgeWhenEmpty extends PropertyBehaviour
}

case class Property(
  name: String,
  behaviour: PropertyBehaviour
)

object Property {
  def apply(name: String): Property =
    Property(name, PropertyBehaviour.Normal)
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

  private def nonRepeatedHistory(sectionIndex: Int) =
    List.fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))

  private def nonRepeatedATLHistory(pageIndex: Int, sectionIndex: Int) =
    List.fill(pageIndex)(MoveRight) ::: List(DownArray, DownField("pages")) :::
      List.fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))

  private def taskListHistory(sectionIndex: Int, taskIndex: Int, taskSectionIndex: Int) =
    List.fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections")) :::
      List.fill(taskIndex)(MoveRight) ::: List(DownArray, DownField("tasks")) :::
      List.fill(taskSectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))

  private def taskListATLHistory(pageIndex: Int, sectionIndex: Int, taskIndex: Int, taskSectionIndex: Int) =
    List.fill(pageIndex)(MoveRight) ::: List(DownArray, DownField("pages")) :::
      List.fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections")) :::
      List.fill(taskIndex)(MoveRight) ::: List(DownArray, DownField("tasks")) :::
      List.fill(taskSectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))

  private val sectionPattern: Regex = """^\.sections\[(\d+)\]""".r
  private val atlPagePattern: Regex = """^\.sections\[(\d+)\]\.pages\[(\d+)\]$""".r
  private val taskListSectionPattern: Regex = """^\.sections\[(\d+)\]\.tasks\[(\d+)\]\.sections\[(\d+)\]$""".r
  private val taskListAtlPagePattern: Regex =
    """^\.sections\[(\d+)\]\.tasks\[(\d+)\]\.sections\[(\d+)\]\.pages\[(\d+)\]$""".r

  def modifySectionData(json: Json, sectionPath: String, sectionData: Json): Json = {
    val history: List[CursorOp] =
      sectionPath match {
        case sectionPattern(sectionIndex) =>
          nonRepeatedHistory(sectionIndex.toInt)
        case atlPagePattern(sectionIndex, pageIndex) =>
          nonRepeatedATLHistory(pageIndex.toInt, sectionIndex.toInt)
        case taskListSectionPattern(taskSectionIndex, taskIndex, sectionIndex) =>
          taskListHistory(sectionIndex.toInt, taskIndex.toInt, taskSectionIndex.toInt)
        case taskListAtlPagePattern(taskSectionIndex, taskIndex, sectionIndex, pageIndex) =>
          taskListATLHistory(pageIndex.toInt, sectionIndex.toInt, taskIndex.toInt, taskSectionIndex.toInt)
        case _ =>
          Nil
      }
    List(
      Property("title"),
      Property("caption"),
      Property("description"),
      Property("shortName", PropertyBehaviour.PurgeWhenEmpty),
      Property("continueLabel"),
      Property("presentationHint", PropertyBehaviour.PurgeWhenEmpty),
      Property("note", PropertyBehaviour.PurgeWhenEmpty)
    ).foldRight(json) { case (property, accJson) =>
      sectionData.hcursor
        .downField(property.name)
        .focus
        .flatMap { propertyValue =>
          val array = accJson.hcursor.replay(history)

          val isValueAsStringEmpty = propertyValue
            .as[String]
            .toOption
            .fold(false)(_.trim.isEmpty())

          array.success.flatMap { hcursor =>
            property.behaviour match {
              case PropertyBehaviour.PurgeWhenEmpty if isValueAsStringEmpty =>
                hcursor
                  .downField(property.name)
                  .delete
                  .root
                  .focus
              case _ =>
                val propertyField = hcursor.downField(property.name)
                if (propertyField.succeeded) {
                  propertyField
                    .set(propertyValue)
                    .root
                    .focus
                } else {
                  hcursor.withFocus(json => json.deepMerge(Json.obj(property.name -> propertyValue))).root.focus
                }
            }
          }
        }
        .getOrElse(accJson)
    }
  }

  def updateFormComponent(
    formComponent: Json,
    sectionData: Json,
    formComponentId: FormComponentId
  ): Json =
    List(
      Property("label"),
      Property("helpText", PropertyBehaviour.PurgeWhenEmpty),
      Property("shortName", PropertyBehaviour.PurgeWhenEmpty),
      Property("format", PropertyBehaviour.PurgeWhenEmpty),
      Property("errorShortName", PropertyBehaviour.PurgeWhenEmpty),
      Property("errorShortNameStart", PropertyBehaviour.PurgeWhenEmpty),
      Property("errorExample", PropertyBehaviour.PurgeWhenEmpty),
      Property("errorMessage", PropertyBehaviour.PurgeWhenEmpty),
      Property("displayWidth", PropertyBehaviour.PurgeWhenEmpty),
      Property("labelSize", PropertyBehaviour.PurgeWhenEmpty),
      Property("mandatory", PropertyBehaviour.PurgeWhenEmpty),
      Property("multiline", PropertyBehaviour.PurgeWhenEmpty),
      Property("infoText"),
      Property("infoType"),
      Property("dividerPosition", PropertyBehaviour.PurgeWhenEmpty),
      Property("dividerText", PropertyBehaviour.PurgeWhenEmpty),
      Property("noneChoice", PropertyBehaviour.PurgeWhenEmpty),
      Property("noneChoiceError", PropertyBehaviour.PurgeWhenEmpty),
      Property("multivalue", PropertyBehaviour.PurgeWhenEmpty),
      Property("choices", PropertyBehaviour.PurgeWhenEmpty)
    )
      .foldRight(formComponent) { case (property, accJson) =>
        sectionData.hcursor
          .downField(property.name)
          .focus
          .flatMap { propertyValue =>
            val isValueAsStringEmpty = propertyValue
              .as[String]
              .toOption
              .fold(false)(_.trim.isEmpty())

            val propertyField = accJson.hcursor.downField(property.name)

            property.behaviour match {
              case PropertyBehaviour.PurgeWhenEmpty if isValueAsStringEmpty =>
                if (propertyField.succeeded) {
                  propertyField.delete.root.focus
                } else {
                  Some(accJson)
                }

              case _ =>
                if (propertyField.succeeded) {
                  propertyField
                    .set(propertyValue)
                    .root
                    .focus

                } else {
                  accJson.hcursor
                    .withFocus(json => json.deepMerge(Json.obj(property.name -> propertyValue)))
                    .root
                    .focus
                }
            }
          }
          .getOrElse(accJson)
      }

  def modifyFormComponentData(
    json: Json,
    formComponentId: FormComponentId,
    sectionData: Json
  ): Json = {

    val id = Json.fromString(formComponentId.value)
    val maybeHistory = json.hcursor
      .downField("sections")
      .values
      .flatMap { sections =>
        val histories: Iterable[Option[List[CursorOp]]] = sections.zipWithIndex.map { case (section, sectionIndex) =>
          val fieldHit: Option[List[CursorOp]] = {
            val historySuffix = nonRepeatedHistory(sectionIndex)
            fieldHistory(section, id, historySuffix)
          }

          val addToListHit: List[Option[List[CursorOp]]] =
            section.hcursor.downField("pages").values.toList.flatMap { pages =>
              pages.zipWithIndex.map { case (page, pageHitIndex) =>
                val historySuffix = nonRepeatedATLHistory(pageHitIndex, sectionIndex)
                fieldHistory(page, id, historySuffix)
              }
            }

          val taskListHit: List[Option[List[CursorOp]]] =
            section.hcursor.downField("tasks").values.toList.flatMap { tasks =>
              tasks.zipWithIndex.flatMap { case (task, taskHitIndex) =>
                task.hcursor.downField("sections").values.toList.flatMap { sections =>
                  sections.zipWithIndex.map { case (section, sectionHitIndex) =>
                    val historySuffix = taskListHistory(sectionHitIndex, taskHitIndex, sectionIndex)
                    fieldHistory(section, id, historySuffix)
                  }
                }
              }
            }

          val taskListAddToListHit: List[Option[List[CursorOp]]] =
            section.hcursor.downField("tasks").values.toList.flatMap { tasks =>
              tasks.zipWithIndex.flatMap { case (task, taskHitIndex) =>
                task.hcursor.downField("sections").values.toList.flatMap { sections =>
                  sections.zipWithIndex.flatMap { case (section, sectionHitIndex) =>
                    section.hcursor.downField("pages").values.toList.flatMap { pages =>
                      pages.zipWithIndex.map { case (page, pageHitIndex) =>
                        val historySuffix =
                          taskListATLHistory(pageHitIndex, sectionHitIndex, taskHitIndex, sectionIndex)
                        fieldHistory(page, id, historySuffix)
                      }
                    }
                  }
                }
              }
            }

          (fieldHit :: addToListHit ::: taskListHit ::: taskListAddToListHit).collectFirst { case Some(history) =>
            history
          }
        }

        histories.collectFirst { case Some(history) => history }
      }

    maybeHistory
      .flatMap { history =>
        json.hcursor
          .replay(history)
          .withFocus(field => updateFormComponent(field, sectionData, formComponentId))
          .root
          .focus
      }
      .getOrElse(json)
  }

  private def fieldHistory(json: Json, id: Json, historySuffix: List[CursorOp]): Option[List[CursorOp]] =
    json.hcursor.downField("fields").values.flatMap { fields =>
      fields.zipWithIndex
        .find { case (field, _) =>
          field.hcursor.downField("id").focus.contains(id)
        }
        .map { case (_, fieldHitIndex) =>
          List.fill(fieldHitIndex)(MoveRight) ::: List(DownArray, DownField("fields")) ::: historySuffix
        }
    }

  def updateFormComponent(
    formTemplateRaw: FormTemplateRaw,
    sectionData: Json,
    formComponentId: FormComponentId
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifyFormComponentData(_, formComponentId, sectionData))

  def updateSectionByPath(
    formTemplateRaw: FormTemplateRaw,
    sectionPath: String,
    sectionData: Json
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifySectionData(_, sectionPath, sectionData))
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

  def updateSection(formTemplateRawId: FormTemplateRawId, sectionPath: String) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .updateSectionByPath(formTemplateRaw, sectionPath, requestBody)
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }

  def updateFormComponent(formTemplateRawId: FormTemplateRawId, formComponentId: FormComponentId) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .updateFormComponent(formTemplateRaw, requestBody, formComponentId)
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }
}
