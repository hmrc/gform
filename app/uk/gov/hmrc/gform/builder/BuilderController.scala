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
  // Custom logic for "choices" field
  case object Choices extends PropertyBehaviour
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
      Property("displayWidth", PropertyBehaviour.PurgeWhenEmpty),
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

  private def updateChoiceLang(propertyValue: Json, accJson: Json): Json = {
    val maybeIncomingEn = propertyValue.hcursor.downField("en").focus
    val maybeTemplateEn = if (accJson.isString) Some(accJson) else accJson.hcursor.downField("en").focus

    if (maybeIncomingEn === maybeTemplateEn) {
      if (accJson.isString) {
        Json.obj("en" := accJson)
      } else {
        accJson
      }
    } else {
      maybeIncomingEn.fold(accJson) { incomingEn =>
        if (accJson.isString) {
          Json.obj("en" := incomingEn)
        } else {
          accJson.hcursor
            .downField("cy")
            .delete
            .root
            .downField("en")
            .set(incomingEn)
            .root
            .focus
            .getOrElse(accJson)
        }

      }
    }
  }

  private def updateIncludeIf(propertyValue: Json, accJson: Json): Json = {
    val maybeIncomingIncludeIf = propertyValue.hcursor.downField("includeIf").focus
    val isValueAsStringEmpty = maybeIncomingIncludeIf.flatMap(_.as[String].toOption).fold(false)(_.trim.isEmpty())

    maybeIncomingIncludeIf match {
      case Some(incomingIncludeIf) if !isValueAsStringEmpty =>
        val includeIfCursor = accJson.hcursor.downField("includeIf")

        if (includeIfCursor.succeeded) {
          includeIfCursor.set(incomingIncludeIf).root.focus.getOrElse(accJson)
        } else {
          Json.obj("includeIf" := incomingIncludeIf).deepMerge(accJson)
        }

      case _ =>
        accJson.hcursor.downField("includeIf").delete.root.focus.getOrElse(accJson)
    }
  }

  private def updateHint(propertyValue: Json, accJson: Json): Json = {
    val maybeIncomingHints = propertyValue.hcursor.downField("hint").focus
    val maybeTemplateHints = accJson.hcursor.downField("hint").focus

    val isValueAsStringEmpty = maybeIncomingHints.flatMap(_.as[String].toOption).fold(false)(_.trim.isEmpty())

    maybeIncomingHints match {
      case Some(incomingHint) if !isValueAsStringEmpty =>
        maybeTemplateHints match {
          case Some(existingHint) =>
            val localisedValue: ACursor = existingHint.hcursor.downField("en")
            val en: Option[Json] = if (localisedValue.succeeded) {
              localisedValue.focus
            } else {
              Some(existingHint)
            }
            if (en.contains(incomingHint)) {
              accJson
            } else {
              accJson.hcursor.downField("hint").set(incomingHint).root.focus.getOrElse(accJson)
            }
          case None =>
            accJson.deepMerge(Json.obj("hint" := incomingHint))
        }

      case _ =>
        accJson.hcursor.downField("hint").delete.root.focus.getOrElse(accJson)
    }
  }

  private def updateChoices(browserValue: Json, templateValue: Json): Json = {
    val templateValueUpd1 = updateChoiceLang(browserValue, templateValue)
    val templateValueUpd2 = updateHint(browserValue, templateValueUpd1)
    val templateValueUpd3 = updateIncludeIf(browserValue, templateValueUpd2)

    templateValueUpd3
  }

  private def isSmartStringObj(s: Json): Boolean =
    s.asObject.fold(false) { obj =>
      obj.contains("en")
    }

  private def isSmartStringArray(s: Json): Boolean =
    s.asArray.fold(false) { smartStringConds =>
      smartStringConds.forall(isSmartStringObj)
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
        case PropertyBehaviour.PurgeWhenEmpty | PropertyBehaviour.Choices if isValueAsStringEmpty =>
          hcursor
            .downField(property.name)
            .delete
            .root
            .focus

        case PropertyBehaviour.Choices =>
          val browserValues: Option[Iterable[Json]] = propertyValue.hcursor.values
          val templateValues: Option[Iterable[Json]] = hcursor.downField(property.name).values

          val choices: Json = (browserValues, templateValues) match {
            case (Some(browserValues), Some(templateValues)) =>
              val res = if (browserValues.size === templateValues.size) {
                browserValues.zip(templateValues).map { case (browserValue, templateValue) =>
                  updateChoices(browserValue, templateValue)
                }
              } else {
                // This case covers using Undo on YesNo choice which originally didn't have 2 choices
                browserValues.map { browserValue =>
                  updateChoices(browserValue, browserValue)
                }
              }
              Json.arr(res.toList: _*)
            case (Some(browserValues), None) =>
              val res = browserValues.map { browserValue =>
                val templateValue = Json.fromString("")
                updateChoices(browserValue, templateValue)
              }
              Json.arr(res.toList: _*)

            case _ => accJson
          }

          val choicesCursor = hcursor.downField(property.name) // This can fail when migration away from 'yesno'

          if (choicesCursor.succeeded) {
            choicesCursor
              .set(choices)
              .root
              .focus
          } else {
            hcursor.focus.map(obj => Json.obj("choices" := choices).deepMerge(obj))
          }

        case _ =>
          val propertyField = hcursor.downField(property.name)
          if (propertyField.succeeded) {
            val isSmartStringArrayPropertyValue = isSmartStringArray(propertyValue)
            if (isSmartStringArrayPropertyValue) {
              (propertyValue.hcursor.values, propertyField.values) match {
                case (Some(incomingValues), Some(templateValues)) =>
                  val updates = incomingValues.zip(templateValues).map { case (incoming, existing) =>
                    val upd1 = modifyEn(incoming, existing)
                    updateIncludeIf(incoming, upd1)
                  }
                  propertyField.set(Json.arr(updates.toList: _*)).root.focus
                case _ => propertyField.root.focus
              }
            } else {
              val localisedValue: ACursor = propertyField.replayOne(DownField("en"))
              val oldValue = if (localisedValue.succeeded) {
                localisedValue.focus
              } else {
                propertyField.focus
              }

              val isSmartStringObjPropertyValue = isSmartStringObj(propertyValue)
              if (isSmartStringObjPropertyValue) {
                propertyField.focus.flatMap { pf =>
                  val updatedPropertryValue = updateChoiceLang(propertyValue, pf)
                  propertyField
                    .set(updatedPropertryValue)
                    .root
                    .focus
                }
              } else if (oldValue.contains(propertyValue)) {
                propertyField.root.focus // Do not update what didn't change, to keep welsch untouched
              } else {
                propertyField
                  .set(propertyValue)
                  .root
                  .focus
              }
            }
          } else {
            hcursor.withFocus(json => json.deepMerge(Json.obj(property.name -> propertyValue))).root.focus
          }
      }
    }
  }

  private def modifyEn(incoming: Json, existing: Json): Json = {
    val exEn: ACursor = existing.hcursor.downField("en")
    val incomingEn: ACursor = incoming.hcursor.downField("en")
    if (exEn.focus === incomingEn.focus) {
      existing
    } else {
      incomingEn.focus.fold(existing) { ie =>
        exEn.set(ie).up.downField("cy").delete.root.focus.getOrElse(existing)
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

  //TODO: Update (removal)
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
        Property("choices", PropertyBehaviour.Choices),
        Property("cityMandatory", PropertyBehaviour.PurgeWhenEmpty),
        Property("countyDisplayed", PropertyBehaviour.PurgeWhenEmpty),
        Property("line2Mandatory", PropertyBehaviour.PurgeWhenEmpty),
        Property("postcodeMandatory", PropertyBehaviour.PurgeWhenEmpty),
        Property("countryLookup", PropertyBehaviour.PurgeWhenEmpty),
        Property("priority", PropertyBehaviour.PurgeWhenEmpty),
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

  // Top level hints are deprecated, move them into choices
  private def moveHints(json: Json): Json = {
    val tpe = json.hcursor.downField("type").focus
    val choice = Json.fromString("choice")
    val revealingChoice = Json.fromString("revealingChoice")
    if (tpe.contains(choice) || tpe.contains(revealingChoice)) {

      val maybeHints: Option[Iterable[Json]] = json.hcursor.downField("hints").values
      val maybeChoices: Option[Iterable[Json]] = json.hcursor.downField("choices").values

      (maybeHints, maybeChoices) match {
        case (Some(hints), Some(choices)) =>
          val merged: Iterable[io.circe.Json] = hints.zip(choices).map { case (hint, choice) =>
            val choiceNormalised = if (choice.isString) {
              Json.obj("en" := choice)
            } else {
              choice.hcursor.downField("hint").delete.root.focus.getOrElse(choice)
            }
            // Top level hint has priority over choice's hint
            Json.obj("hint" := hint).deepMerge(choiceNormalised)
          }
          val choicesUpd = Json.arr(merged.toList: _*)

          json.hcursor
            .downField("hints") // Delete top level hints
            .delete
            .root
            .downField("choices")
            .set(choicesUpd)
            .root
            .focus
            .getOrElse(json)

        case _ => json
      }
    } else {
      json
    }
  }

  private def patchFormComponent(maybeHistory: Option[List[CursorOp]], json: Json, formComponentData: Json): Json =
    maybeHistory
      .flatMap { history =>
        json.hcursor
          .replay(history)
          .withFocus { field =>
            val fieldUpd = moveHints(field)
            updateFormComponent(fieldUpd, formComponentData)
          }
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
          requestBody.as[Json].leftMap(e => BuilderError.CirceDecodingError(e))
        formTemplateRaw <- BuilderSupport
                             .updateAtlRepeaterFormComponent(
                               formTemplateRaw,
                               componentUpdateRequest,
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
        componentUpdateRequest <- requestBody.as[Json].leftMap(e => BuilderError.CirceDecodingError(e))
        formTemplateRaw <- BuilderSupport
                             .updateAtlDefaultPageFormComponent(
                               formTemplateRaw,
                               componentUpdateRequest,
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
          requestBody.as[Json].leftMap(e => BuilderError.CirceDecodingError(e))
        formTemplateRaw <-
          BuilderSupport
            .updateSummarySectionFormComponent(
              formTemplateRaw,
              componentUpdateRequest,
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
          requestBody.as[Json].leftMap(e => BuilderError.CirceDecodingError(e))

        formTemplateRaw <-
          BuilderSupport
            .updateAcknowledgementFormComponent(
              formTemplateRaw,
              componentUpdateRequest,
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

final case class FormTemplateUpdateRequest(formTemplate: Json)

object FormTemplateUpdateRequest {
  implicit val formTemplateUpdateRequest: Decoder[FormTemplateUpdateRequest] = deriveDecoder[FormTemplateUpdateRequest]
}
