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
import io.circe.syntax._
import io.circe.CursorOp._
import io.circe.Json
import play.api.libs.circe.Circe
import play.api.libs.json.{ JsError, JsObject, JsString, JsSuccess }
import play.api.libs.json.{ Json => PlayJson }
import play.api.mvc.{ ControllerComponents, Result, Results }
import scala.concurrent.Future
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, FormTemplatesControllerRequestHandler, RequestHandlerAlg }
import uk.gov.hmrc.gform.history.HistoryService
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import io.circe.generic.semiauto._

import scala.concurrent.ExecutionContext

sealed trait BuilderError extends Product with Serializable

object BuilderError {
  final case class CirceError(failure: ParsingFailure) extends BuilderError
  final case class CirceDecodingError(failure: DecodingFailure) extends BuilderError
  final case class OtherBuilderError(message: String) extends BuilderError
  final case class PlayJsonError(message: String) extends BuilderError
}

sealed trait PropertyBehaviour extends Product with Serializable

object PropertyBehaviour {
  // Set property to whatever value has been received from the client
  case object Normal extends PropertyBehaviour
  // Remove property if received value from client is empty
  case object PurgeWhenEmpty extends PropertyBehaviour
  // Remove property if received value from client is empty
  case object StringArrayButPurgeWhenEmpty extends PropertyBehaviour
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

  def modifyJson(
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

  def modifyAtlDefaultPageData(
    json: Json,
    atlRepeaterSection: Json,
    sectionPath: SectionPath
  ): Json = {
    val propertyList = List(
      Property("note", PropertyBehaviour.PurgeWhenEmpty),
      Property("doneNote", PropertyBehaviour.PurgeWhenEmpty),
      Property("caption", PropertyBehaviour.PurgeWhenEmpty),
      Property("continueLabel", PropertyBehaviour.PurgeWhenEmpty),
      Property("title")
    )

    modifyAtlRepeater(
      propertyList,
      json,
      atlRepeaterSection,
      Some(DownField("defaultPage")),
      sectionPath
    )
  }

  def modifyAtlCyaPageData(
    json: Json,
    atlRepeaterSection: Json,
    sectionPath: SectionPath
  ): Json = {
    val propertyList = List(
      Property("note", PropertyBehaviour.PurgeWhenEmpty),
      Property("doneNote", PropertyBehaviour.PurgeWhenEmpty),
      Property("caption", PropertyBehaviour.PurgeWhenEmpty),
      Property("continueLabel", PropertyBehaviour.PurgeWhenEmpty),
      Property("title", PropertyBehaviour.PurgeWhenEmpty),
      Property("updateTitle"),
      Property("noPIITitle", PropertyBehaviour.PurgeWhenEmpty),
      Property("noPIIUpdateTitle", PropertyBehaviour.PurgeWhenEmpty),
      Property("presentationHint", PropertyBehaviour.PurgeWhenEmpty),
      Property("removeItemIf", PropertyBehaviour.PurgeWhenEmpty),
      Property("header", PropertyBehaviour.PurgeWhenEmpty),
      Property("footer", PropertyBehaviour.PurgeWhenEmpty)
    )

    modifyAtlRepeater(
      propertyList,
      json,
      atlRepeaterSection,
      Some(DownField("cyaPage")),
      sectionPath
    )
  }

  def modifyAtlRepeaterData(
    json: Json,
    atlRepeaterSection: Json,
    sectionPath: SectionPath
  ): Json = {
    val propertyList = List(
      Property("note", PropertyBehaviour.PurgeWhenEmpty),
      Property("doneNote", PropertyBehaviour.PurgeWhenEmpty),
      Property("caption", PropertyBehaviour.PurgeWhenEmpty),
      Property("description"),
      Property("title"),
      Property("shortName"),
      Property("summaryName"),
      Property("summaryDescription"),
      Property("defaultPage", PropertyBehaviour.PurgeWhenEmpty),
      Property("cyaPage", PropertyBehaviour.PurgeWhenEmpty),
      Property("repeatsWhile", PropertyBehaviour.PurgeWhenEmpty),
      Property("repeatsUntil", PropertyBehaviour.PurgeWhenEmpty),
      Property("pageIdToDisplayAfterRemove", PropertyBehaviour.PurgeWhenEmpty),
      Property("presentationHint", PropertyBehaviour.PurgeWhenEmpty),
      Property("continueLabel", PropertyBehaviour.PurgeWhenEmpty)
    )

    modifyAtlRepeater(
      propertyList,
      json,
      atlRepeaterSection,
      None,
      sectionPath
    )
  }

  def modifyTaskSection(json: Json, taskSectionData: Json, sectionPath: SectionPath): Json = {
    val propertyList = List(
      Property("title")
    )

    val history = sectionPath.asHistory
    updateJsonByPropertyList(propertyList, json, taskSectionData, history)

  }

  def modifyTaskSummarySection(json: Json, taskSectionSummaryData: Json, sectionPath: SectionPath): Json = {
    val propertyList = List(
      Property("title"),
      Property("header"),
      Property("footer")
    )

    val isTitleEmpty: Boolean =
      taskSectionSummaryData.hcursor.downField("title").as[String].toOption.fold(false)(_.trim.isEmpty())

    val history = sectionPath.asHistory

    if (isTitleEmpty) {
      json.hcursor
        .replay(history)
        .downField("summarySection")
        .delete
        .root
        .focus
        .getOrElse(json)
    } else {
      val summarySectionHistory = DownField("summarySection") :: history
      val noTaskSummarySection = json.hcursor.replay(summarySectionHistory).failed
      if (noTaskSummarySection) {
        val summarySection = Json.obj("header" := "", "footer" := "").deepMerge(taskSectionSummaryData)
        json.hcursor
          .replay(history)
          .withFocus(json => json.deepMerge(Json.obj("summarySection" := summarySection)))
          .top
          .getOrElse(json)
      } else {
        updateJsonByPropertyList(propertyList, json, taskSectionSummaryData, summarySectionHistory)
      }
    }
  }

  def modifyTask(json: Json, taskSectionData: Json, sectionPath: SectionPath): Json = {
    val propertyList = List(
      Property("title"),
      Property("caption", PropertyBehaviour.PurgeWhenEmpty)
    )

    val history = sectionPath.asHistory
    updateJsonByPropertyList(propertyList, json, taskSectionData, history)
  }

  def modifyAtlRepeaterDataAddAnotherQuestion(
    json: Json,
    atlRepeaterSection: Json,
    sectionPath: SectionPath
  ): Json = {
    val propertyList = List(
      Property("label"),
      Property("errorMessage", PropertyBehaviour.PurgeWhenEmpty)
    )
    modifyAtlRepeater(
      propertyList,
      json,
      atlRepeaterSection,
      Some(DownField("addAnotherQuestion")),
      sectionPath
    )
  }

  def modifyAtlRepeaterFormComponentData(
    json: Json,
    atlRepeaterFormComponent: Json,
    formComponentId: FormComponentId,
    sectionPath: SectionPath
  ): Json = {

    val history = sectionPath.asHistory
    updateFormComponentWithHistory(json, formComponentId, atlRepeaterFormComponent, history)

  }
  def modifyAtlDefaultPageFormComponentData(
    json: Json,
    atlRepeaterFormComponent: Json,
    formComponentId: FormComponentId,
    sectionPath: SectionPath
  ): Json = {

    val history = DownField("defaultPage") :: sectionPath.asHistory
    updateFormComponentWithHistory(json, formComponentId, atlRepeaterFormComponent, history)

  }

  private def modifyAtlRepeater(
    propertyList: List[Property],
    json: Json,
    atlRepeaterSection: Json,
    historySuffix: Option[CursorOp],
    sectionPath: SectionPath
  ) = {
    val history = sectionPath.asHistory
    updateJsonByPropertyList(propertyList, json, atlRepeaterSection, historySuffix.toList ::: history)
  }

  def modifySummarySectionData(json: Json, summarySection: Json, maybeCoordinates: Option[Coordinates]): Json =
    maybeCoordinates match {
      case Some(coordinates) =>
        val history = List(DownField("summarySection")) ++
          SectionPath.taskHistory(coordinates.taskSectionNumber.value, coordinates.taskNumber.value)
        updateSummary(json, summarySection, history)
      case None =>
        val noSummarySection: Boolean = json.hcursor.downField("summarySection").failed

        val jsonWithSummarySection = if (noSummarySection) {

          val formCategory = json.hcursor.downField("formCategory").focus.flatMap(_.asString) match {
            case None               => Default
            case Some(formCategory) => FormCategory.fromString(formCategory).getOrElse(Default)
          }

          io.circe.parser.parse(PlayJson.stringify(SummarySection.defaultJson(formCategory))) match {
            case Left(_) => json
            case Right(defaultSummarySection) =>
              json.deepMerge(Json.obj("summarySection" -> defaultSummarySection))
          }
        } else {
          json
        }
        val history = List(DownField("summarySection"))
        updateSummary(jsonWithSummarySection, summarySection, history)
    }

  private def modifyAcknowledgementData(json: Json, acknowledgement: Json): Json = {
    val history = List(DownField("acknowledgementSection"))

    val propertyList = List(
      Property("title"),
      Property("panelTitle", PropertyBehaviour.PurgeWhenEmpty),
      Property("showReference"),
      Property("note", PropertyBehaviour.PurgeWhenEmpty),
      Property("doneNote", PropertyBehaviour.PurgeWhenEmpty)
    )

    updateJsonByPropertyList(propertyList, json, acknowledgement, history)
  }

  private def updateSummary(json: Json, summarySection: Json, history: List[CursorOp]): Json = {
    val propertyList = List(
      Property("note", PropertyBehaviour.PurgeWhenEmpty),
      Property("doneNote", PropertyBehaviour.PurgeWhenEmpty),
      Property("title"),
      Property("header"),
      Property("footer"),
      Property("continueLabel", PropertyBehaviour.PurgeWhenEmpty),
      Property("displayWidth", PropertyBehaviour.PurgeWhenEmpty)
    )

    updateJsonByPropertyList(propertyList, json, summarySection, history)
  }

  private def updateProperty(
    property: Property,
    propertyValue: Json,
    history: List[CursorOp],
    accJson: Json
  ): Option[Json] = {
    val target = accJson.hcursor.replay(history)

    val isValueAsStringEmpty = propertyValue
      .as[String]
      .toOption
      .fold(false)(_.trim.isEmpty())

    target.success.flatMap { hcursor =>
      property.behaviour match {
        case PropertyBehaviour.PurgeWhenEmpty | PropertyBehaviour.StringArrayButPurgeWhenEmpty
            if isValueAsStringEmpty =>
          hcursor
            .downField(property.name)
            .delete
            .root
            .focus
        case PropertyBehaviour.StringArrayButPurgeWhenEmpty =>
          val propertyField = hcursor.downField(property.name)
          if (propertyField.succeeded) {
            val enLabelsOnly: Option[Json] = propertyField.values.map { values =>
              val enOnly = values.flatMap { value =>
                val localisedValue: ACursor = value.hcursor.replayOne(DownField("en"))
                if (localisedValue.succeeded) {
                  localisedValue.focus
                } else {
                  propertyField.focus
                }
              }
              Json.arr(enOnly.toList: _*)
            }

            if (enLabelsOnly.contains(propertyValue)) {
              hcursor.focus
            } else {
              propertyField
                .set(propertyValue)
                .root
                .focus
            }
          } else {
            hcursor.withFocus(json => json.deepMerge(Json.obj(property.name -> propertyValue))).root.focus
          }
        case _ =>
          val propertyField = hcursor.downField(property.name)
          if (propertyField.succeeded) {
            val localisedValue: ACursor = propertyField.replayOne(DownField("en"))
            val oldValue = if (localisedValue.succeeded) {
              localisedValue.focus
            } else {
              propertyField.focus
            }
            if (oldValue.contains(propertyValue)) {
              propertyField.root.focus // Do not update what didn't change, to keep welsch untouched
            } else {
              propertyField
                .set(propertyValue)
                .root
                .focus
            }
          } else {
            hcursor.withFocus(json => json.deepMerge(Json.obj(property.name -> propertyValue))).root.focus
          }
      }
    }
  }

  def modifySectionData(json: Json, sectionPath: SectionPath, sectionData: Json): Json = {
    val history: List[CursorOp] = sectionPath.asHistory
    val propertyList = List(
      Property("title"),
      Property("caption", PropertyBehaviour.PurgeWhenEmpty),
      Property("description", PropertyBehaviour.PurgeWhenEmpty),
      Property("shortName", PropertyBehaviour.PurgeWhenEmpty),
      Property("continueLabel", PropertyBehaviour.PurgeWhenEmpty),
      Property("presentationHint", PropertyBehaviour.PurgeWhenEmpty),
      Property("note", PropertyBehaviour.PurgeWhenEmpty),
      Property("doneNote", PropertyBehaviour.PurgeWhenEmpty),
      Property("summarySection", PropertyBehaviour.PurgeWhenEmpty)
    )

    updateJsonByPropertyList(propertyList, json, sectionData, history)

  }

  def updateFormComponent(
    formComponent: Json,
    sectionData: Json
  ): Json = {
    val propertyList =
      List(
        Property("type"),
        Property("label", PropertyBehaviour.PurgeWhenEmpty),
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
        Property("choices", PropertyBehaviour.StringArrayButPurgeWhenEmpty),
        Property("hints", PropertyBehaviour.StringArrayButPurgeWhenEmpty),
        Property("cityMandatory", PropertyBehaviour.PurgeWhenEmpty),
        Property("countyDisplayed", PropertyBehaviour.PurgeWhenEmpty),
        Property("line2Mandatory", PropertyBehaviour.PurgeWhenEmpty),
        Property("postcodeMandatory", PropertyBehaviour.PurgeWhenEmpty),
        Property("countryLookup", PropertyBehaviour.PurgeWhenEmpty),
        Property("countryDisplayed", PropertyBehaviour.PurgeWhenEmpty)
      )

    updateJsonByPropertyList(propertyList, formComponent, sectionData, List.empty)
  }

  private def updateJsonByPropertyList(
    propertyList: List[Property],
    json: Json,
    updates: Json,
    history: List[CursorOp]
  ): Json =
    propertyList.foldRight(json) { case (property, accJson) =>
      updates.hcursor
        .downField(property.name)
        .focus
        .flatMap { propertyValue =>
          updateProperty(property, propertyValue, history, accJson)
        }
        .getOrElse(accJson)
    }

  def modifyFormTemplate(
    json: Json,
    formTemplateData: Json
  ): Json = {
    val propertyList = List(
      Property("displayWidth", PropertyBehaviour.PurgeWhenEmpty),
      Property("submitSection", PropertyBehaviour.PurgeWhenEmpty),
      Property("note", PropertyBehaviour.PurgeWhenEmpty),
      Property("doneNote", PropertyBehaviour.PurgeWhenEmpty)
    )

    updateJsonByPropertyList(propertyList, json, formTemplateData, List.empty[CursorOp])
  }

  def modifySubmitSection(
    json: Json,
    submitSectionData: Json
  ): Json = {
    val propertyList = List(
      Property("label", PropertyBehaviour.PurgeWhenEmpty), // Fail to insert with empty label
      Property("taskLabel", PropertyBehaviour.PurgeWhenEmpty) // Fail to insert with empty task label
    )
    val history = List(DownField("submitSection"))

    updateJsonByPropertyList(propertyList, json, submitSectionData, history)
  }

  def modifyFormComponentData(
    json: Json,
    formComponentId: FormComponentId,
    formComponentData: Json
  ): Json = {

    val id = Json.fromString(formComponentId.value)
    val maybeHistory = json.hcursor
      .downField("sections")
      .values
      .flatMap { sections =>
        val histories: Iterable[Option[List[CursorOp]]] = sections.zipWithIndex.map { case (section, sectionIndex) =>
          val fieldHit: Option[List[CursorOp]] = {
            val historySuffix = SectionPath.nonRepeatedHistory(sectionIndex)
            fieldHistory(section, id, historySuffix)
          }

          val addToListHit: List[Option[List[CursorOp]]] =
            section.hcursor.downField("pages").values.toList.flatMap { pages =>
              pages.zipWithIndex.map { case (page, pageHitIndex) =>
                val historySuffix = SectionPath.nonRepeatedATLHistory(pageHitIndex, sectionIndex)
                fieldHistory(page, id, historySuffix)
              }
            }

          val taskListHit: List[Option[List[CursorOp]]] =
            section.hcursor.downField("tasks").values.toList.flatMap { tasks =>
              tasks.zipWithIndex.flatMap { case (task, taskHitIndex) =>
                task.hcursor.downField("sections").values.toList.flatMap { sections =>
                  sections.zipWithIndex.map { case (section, sectionHitIndex) =>
                    val historySuffix = SectionPath.taskListHistory(sectionHitIndex, taskHitIndex, sectionIndex)
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
                          SectionPath.taskListATLHistory(pageHitIndex, sectionHitIndex, taskHitIndex, sectionIndex)
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

    patchFormComponent(maybeHistory, json, formComponentData)

  }

  def modifySummarySectionFormComponentData(
    json: Json,
    formComponentId: FormComponentId,
    formComponentData: Json,
    maybeCoordinates: Option[Coordinates]
  ): Json = {

    val historySuffix = maybeCoordinates match {
      case Some(coordinates) =>
        List(DownField("summarySection")) ++
          SectionPath.taskHistory(coordinates.taskSectionNumber.value, coordinates.taskNumber.value)
      case None =>
        List(DownField("summarySection"))
    }

    updateFormComponentWithHistory(json, formComponentId, formComponentData, historySuffix)

  }

  private def modifyAcknowledgementFormComponentData(
    json: Json,
    formComponentId: FormComponentId,
    formComponentData: Json
  ): Json = {
    val historySuffix = List(DownField("acknowledgementSection"))

    updateFormComponentWithHistory(json, formComponentId, formComponentData, historySuffix)
  }

  private def updateFormComponentWithHistory(
    json: Json,
    formComponentId: FormComponentId,
    formComponentData: Json,
    historySuffix: List[CursorOp]
  ): Json = {
    val id = Json.fromString(formComponentId.value)

    val maybeHistory: Option[List[CursorOp]] = json.hcursor
      .replay(historySuffix)
      .focus
      .flatMap { section =>
        fieldHistory(section, id, historySuffix)
      }

    patchFormComponent(maybeHistory, json, formComponentData)

  }

  private def patchFormComponent(maybeHistory: Option[List[CursorOp]], json: Json, formComponentData: Json): Json =
    maybeHistory
      .flatMap { history =>
        json.hcursor
          .replay(history)
          .withFocus(field => updateFormComponent(field, formComponentData))
          .root
          .focus
      }
      .getOrElse(json)

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
    formComponentData: Json,
    formComponentId: FormComponentId
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifyFormComponentData(_, formComponentId, formComponentData))

  def updateSectionByPath(
    formTemplateRaw: FormTemplateRaw,
    sectionDetails: SectionDetails
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifySectionData(_, SectionPath(sectionDetails.sectionPath), sectionDetails.section))

  def updateFormTemplate(
    formTemplateRaw: FormTemplateRaw,
    formTempateData: Json
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifyFormTemplate(_, formTempateData))

  def updateSummarySection(
    formTemplateRaw: FormTemplateRaw,
    summarySectionData: Json,
    maybeCoordinates: Option[Coordinates]
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifySummarySectionData(_, summarySectionData, maybeCoordinates))

  def updateAtlDefaultPage(
    formTemplateRaw: FormTemplateRaw,
    atlRepeaterData: Json,
    sectionPath: SectionPath
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifyAtlDefaultPageData(_, atlRepeaterData, sectionPath))

  def updateAtlCyaPage(
    formTemplateRaw: FormTemplateRaw,
    atlRepeaterData: Json,
    sectionPath: SectionPath
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifyAtlCyaPageData(_, atlRepeaterData, sectionPath))

  def updateAtlRepeater(
    formTemplateRaw: FormTemplateRaw,
    atlRepeaterData: Json,
    sectionPath: SectionPath
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifyAtlRepeaterData(_, atlRepeaterData, sectionPath))

  def updateAtlRepeaterAddAnotherQuestion(
    formTemplateRaw: FormTemplateRaw,
    atlRepeaterData: Json,
    sectionPath: SectionPath
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(
      modifyAtlRepeaterDataAddAnotherQuestion(_, atlRepeaterData, sectionPath)
    )

  def updateAtlRepeaterFormComponent(
    formTemplateRaw: FormTemplateRaw,
    atlRepeaterData: Json,
    formComponentId: FormComponentId,
    sectionPath: SectionPath
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(
      modifyAtlRepeaterFormComponentData(_, atlRepeaterData, formComponentId, sectionPath)
    )

  def updateAtlDefaultPageFormComponent(
    formTemplateRaw: FormTemplateRaw,
    atlRepeaterData: Json,
    formComponentId: FormComponentId,
    sectionPath: SectionPath
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(
      modifyAtlDefaultPageFormComponentData(_, atlRepeaterData, formComponentId, sectionPath)
    )

  def updateAcknowledgement(
    formTemplateRaw: FormTemplateRaw,
    acknowledgementData: Json
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(modifyAcknowledgementData(_, acknowledgementData))

  def updateSummarySectionFormComponent(
    formTemplateRaw: FormTemplateRaw,
    sectionData: Json,
    formComponentId: FormComponentId,
    maybeCoordinates: Option[Coordinates]
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(
      modifySummarySectionFormComponentData(_, formComponentId, sectionData, maybeCoordinates)
    )

  def updateAcknowledgementFormComponent(
    formTemplateRaw: FormTemplateRaw,
    sectionData: Json,
    formComponentId: FormComponentId
  ): Either[BuilderError, FormTemplateRaw] =
    modifyJson(formTemplateRaw)(
      modifyAcknowledgementFormComponentData(_, formComponentId, sectionData)
    )
}

class BuilderController(
  controllerComponents: ControllerComponents,
  formTemplateService: FormTemplateService,
  historyService: HistoryService
)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) with Circe {

  private val requestHandler: RequestHandlerAlg[FOpt] =
    new FormTemplatesControllerRequestHandler(
      formTemplateService.verifyAndSave,
      formTemplateService.save,
      historyService.save
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
    Action.async(circe.json) { request =>
      applyUpdateFunction(formTemplateRawId)(formTemplateRaw => updateFunction(formTemplateRaw, request.body))
    }

  def updateSummarySection(formTemplateRawId: FormTemplateRawId, maybeCoordinates: Option[Coordinates]) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .updateSummarySection(formTemplateRaw, requestBody, maybeCoordinates)
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }

  def updateAtlRepeaterAddAnotherQuestion(
    formTemplateRawId: FormTemplateRawId,
    sectionPath: SectionPath
  ) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .updateAtlRepeaterAddAnotherQuestion(formTemplateRaw, requestBody, sectionPath)
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }

  def updateAtlDefaultPage(
    formTemplateRawId: FormTemplateRawId,
    sectionPath: SectionPath
  ) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .updateAtlDefaultPage(formTemplateRaw, requestBody, sectionPath)
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }

  def updateAtlCyaPage(
    formTemplateRawId: FormTemplateRawId,
    sectionPath: SectionPath
  ) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .updateAtlCyaPage(formTemplateRaw, requestBody, sectionPath)
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }

  def updateAtlRepeater(
    formTemplateRawId: FormTemplateRawId,
    sectionPath: SectionPath
  ) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .updateAtlRepeater(formTemplateRaw, requestBody, sectionPath)
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }

  def updateAtlRepeaterFormComponent(
    formTemplateRawId: FormTemplateRawId,
    formComponentId: FormComponentId,
    sectionPath: SectionPath
  ) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      for {
        componentUpdateRequest <-
          requestBody.as[ComponentUpdateRequest].leftMap(e => BuilderError.CirceDecodingError(e))
        formTemplateRaw <- BuilderSupport
                             .updateAtlRepeaterFormComponent(
                               formTemplateRaw,
                               componentUpdateRequest.formComponent,
                               formComponentId,
                               sectionPath
                             )
      } yield (formTemplateRaw, Results.Ok(formTemplateRaw.value))
    }

  def updateAtlDefaultPageFormComponent(
    formTemplateRawId: FormTemplateRawId,
    formComponentId: FormComponentId,
    sectionPath: SectionPath
  ) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      for {
        componentUpdateRequest <-
          requestBody.as[ComponentUpdateRequest].leftMap(e => BuilderError.CirceDecodingError(e))
        formTemplateRaw <- BuilderSupport
                             .updateAtlDefaultPageFormComponent(
                               formTemplateRaw,
                               componentUpdateRequest.formComponent,
                               formComponentId,
                               sectionPath
                             )
      } yield (formTemplateRaw, Results.Ok(formTemplateRaw.value))
    }

  def updateSection(formTemplateRawId: FormTemplateRawId) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      for {
        sectionUpdateRequest <-
          requestBody.as[SectionDetails].leftMap(e => BuilderError.CirceDecodingError(e))
        sectionUpdatedFormTemplateRaw <-
          BuilderSupport
            .updateSectionByPath(
              formTemplateRaw,
              sectionUpdateRequest
            )
      } yield (sectionUpdatedFormTemplateRaw, Results.Ok(sectionUpdatedFormTemplateRaw.value))
    }

  def updateFormTemplate(formTemplateRawId: FormTemplateRawId) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      for {
        formTemplateUpdateRequest <-
          requestBody.as[FormTemplateUpdateRequest].leftMap(e => BuilderError.CirceDecodingError(e))
        updatedFormTemplateRaw <-
          BuilderSupport
            .updateFormTemplate(
              formTemplateRaw,
              formTemplateUpdateRequest.formTemplate
            )
      } yield (updatedFormTemplateRaw, Results.Ok(updatedFormTemplateRaw.value))
    }

  def updateSummarySectionFormComponent(
    formTemplateRawId: FormTemplateRawId,
    formComponentId: FormComponentId,
    maybeCoordinates: Option[Coordinates]
  ) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      for {
        componentUpdateRequest <-
          requestBody.as[ComponentUpdateRequest].leftMap(e => BuilderError.CirceDecodingError(e))
        formTemplateRaw <-
          BuilderSupport
            .updateSummarySectionFormComponent(
              formTemplateRaw,
              componentUpdateRequest.formComponent,
              formComponentId,
              maybeCoordinates
            )
      } yield (formTemplateRaw, Results.Ok(formTemplateRaw.value))

    }

  def updateFormComponent(formTemplateRawId: FormTemplateRawId, formComponentId: FormComponentId) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      for {
        componentUpdateRequest <- requestBody.as[Json].leftMap(e => BuilderError.CirceDecodingError(e))
        componentUpdatedFormTemplateRaw <-
          BuilderSupport
            .updateFormComponent(
              formTemplateRaw,
              componentUpdateRequest,
              formComponentId
            )
      } yield (componentUpdatedFormTemplateRaw, Results.Ok(componentUpdatedFormTemplateRaw.value))
    }

  def defaultSummarySection(formCategory: String) =
    Action { request =>
      FormCategory.format.reads(JsString(formCategory)) match {
        case JsSuccess(formCategory, _) => Ok(SummarySection.defaultJson(formCategory))
        case JsError(error)             => BadRequest(JsError.toJson(error).toString())
      }
    }

  def updateAcknowledgement(formTemplateRawId: FormTemplateRawId) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .updateAcknowledgement(formTemplateRaw, requestBody)
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }

  def updateAcknowledgementFormComponent(
    formTemplateRawId: FormTemplateRawId,
    formComponentId: FormComponentId
  ) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      for {
        componentUpdateRequest <-
          requestBody.as[ComponentUpdateRequest].leftMap(e => BuilderError.CirceDecodingError(e))

        formTemplateRaw <-
          BuilderSupport
            .updateAcknowledgementFormComponent(
              formTemplateRaw,
              componentUpdateRequest.formComponent,
              formComponentId
            )
      } yield (formTemplateRaw, Results.Ok(formTemplateRaw.value))
    }

  def updateBatch(formTemplateRawId: FormTemplateRawId) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .modifyJson(formTemplateRaw) { json =>
          requestBody.as[UpdateBatch] match {
            case Right(updateBatch) =>
              val updatedJson = updateBatch.updates.foldRight(json) { case (focusedUpdate, json) =>
                focusedUpdate.focus match {
                  case FocusType.Task =>
                    BuilderSupport.modifyTask(json, focusedUpdate.payload, focusedUpdate.path)
                  case FocusType.TaskSection =>
                    BuilderSupport.modifyTaskSection(json, focusedUpdate.payload, focusedUpdate.path)
                  case FocusType.TaskSummarySection =>
                    BuilderSupport.modifyTaskSummarySection(json, focusedUpdate.payload, focusedUpdate.path)
                  case FocusType.SubmitSection =>
                    BuilderSupport.modifySubmitSection(json, focusedUpdate.payload)
                }
              }
              updatedJson
            case Left(error) => throw new Exception(s"Invalid UpdateBatch: $json, error: $error")
          }
        }
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }

}

final case class UpdateBatch(updates: List[FocusedUpdate])

object UpdateBatch {
  implicit val decodeUpdateBatch: Decoder[UpdateBatch] = deriveDecoder[UpdateBatch]
}

final case class FocusedUpdate(payload: Json, path: SectionPath, focus: FocusType)

object FocusedUpdate {
  implicit val decodeAbc: Decoder[FocusedUpdate] = deriveDecoder[FocusedUpdate]
}

final case class SectionDetails(section: Json, sectionPath: String)

object SectionDetails {
  implicit val decodeSectionDetails: Decoder[SectionDetails] = deriveDecoder[SectionDetails]
}

// TODO Get rid of this, it is not longer needed
final case class ComponentUpdateRequest(formComponent: Json, sectionDetails: Option[SectionDetails])

object ComponentUpdateRequest {
  implicit val componentUpdateRequest: Decoder[ComponentUpdateRequest] = deriveDecoder[ComponentUpdateRequest]
}

final case class FormTemplateUpdateRequest(formTemplate: Json)

object FormTemplateUpdateRequest {
  implicit val formTemplateUpdateRequest: Decoder[FormTemplateUpdateRequest] = deriveDecoder[FormTemplateUpdateRequest]
}
