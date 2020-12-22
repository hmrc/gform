/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.formtemplate

import cats.instances.future._
import cats.syntax.either._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.concurrent.ExecutionContext
import scala.language.postfixOps
import uk.gov.hmrc.gform.core.{ FOpt, Opt, fromOptA }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Default, FormCategory, FormTemplate, FormTemplateRaw, SummarySection }

trait RequestHandlerAlg[F[_]] {
  def handleRequest(templateRaw: FormTemplateRaw): F[Unit]
}

class FormTemplatesControllerRequestHandler[F[_]](
  verifyAndSave: FormTemplate => FOpt[Unit],
  save: FormTemplateRaw => FOpt[Unit])(implicit ec: ExecutionContext) {

  val futureInterpreter = new RequestHandlerAlg[FOpt] {
    override def handleRequest(templateRaw: FormTemplateRaw): FOpt[Unit] = {

      val formTemplateOpt: Opt[FormTemplate] = FormTemplate
        .transformAndReads(templateRaw.value)
        .fold(errors => UnexpectedState(errors.toString()).asLeft, valid => valid.asRight)

      processAndPersistTemplate(formTemplateOpt, templateRaw)
    }
  }

  private def processAndPersistTemplate(formTemplateOpt: Opt[FormTemplate], templateRaw: FormTemplateRaw): FOpt[Unit] =
    for {
      ft <- fromOptA(formTemplateOpt)
      _  <- verifyAndSave(ft)
      _  <- save(templateRaw)
    } yield ()
}

object FormTemplatesControllerRequestHandler {

  val onlyOneOfDestinationsAndPrintSection = JsError(
    """One and only one of FormTemplate.{destinations, printSection} must be defined.""")

  val avoidAcknowledgementForPrintSection = JsError("""Avoid acknowledgement section in case of print section.""")

  val avoidDeclarationForPrintSection = JsError("""Avoid declaration section in case of print section.""")

  val mandatoryAcknowledgementForDestinationSection = JsError(
    """Acknowledgement section is mandatory in case of destination section.""")

  val mandatoryDeclarationForDestinationSection = JsError(
    """Declaration section is mandatory in case of destination section.""")

  val invalidDmsFormId = (id: String) =>
    s"For Destination $id, dmsFormId should have minimum 1 and maximum 12 characters."

  val missingDmsFormId = (id: String) => s"For Destination $id, dmsFormId is missing."

  def normaliseJSON(jsonValue: JsValue): JsResult[JsObject] = {

    val drmValue =
      (__ \ 'draftRetrievalMethod \ 'value).json
        .copyFrom((__ \ 'draftRetrievalMethod).json.pick orElse Reads.pure(JsString("onePerUser")))

    val drmShowContinueOrDeletePage =
      (__ \ 'draftRetrievalMethod \ 'showContinueOrDeletePage).json
        .copyFrom((__ \ 'showContinueOrDeletePage).json.pick orElse Reads.pure(JsString("true")))

    val ensureDisplayHMRCLogo =
      (__ \ 'displayHMRCLogo).json
        .copyFrom((__ \ 'displayHMRCLogo).json.pick orElse Reads.pure(JsTrue))

    val pruneShowContinueOrDeletePage = (__ \ 'showContinueOrDeletePage).json.prune

    val ensureFormCategory =
      (__ \ 'formCategory).json
        .copyFrom((__ \ 'formCategory).json.pick orElse Reads.pure(JsString("default")))

    val ensureLanguages =
      (__ \ 'languages).json
        .copyFrom((__ \ 'languages).json.pick orElse Reads.pure(Json.arr("en")))

    val formCategoryReads: Reads[FormCategory] = (__ \ 'formCategory).json.pick
      .flatMap { jsValue =>
        val formCategoryJsRsult: JsResult[FormCategory] = Reads.of[FormCategory].reads(jsValue)
        Reads.pure(formCategoryJsRsult.getOrElse(Default))
      }
      .orElse(Reads.pure(Default))

    val defaultSummarySection: Reads[JsValue] =
      formCategoryReads.map(formCategory => SummarySection.defaultJson(formCategory))

    val ensureSummarySection =
      (__ \ 'summarySection).json
        .copyFrom((__ \ 'summarySection).json.pick orElse defaultSummarySection)

    val ensureParentFormSubmissionRefs =
      (__ \ 'parentFormSubmissionRefs).json
        .copyFrom((__ \ 'parentFormSubmissionRefs).json.pick orElse Reads.pure(Json.arr()))

    val destinationsOrPrintSection =
      (__ \ 'destinations).json
        .copyFrom((__ \ 'destinations).json.pick orElse (__ \ 'printSection).json.pick orElse Reads.pure(JsString("")))

    val prunePrintSection = (__ \ 'printSection).json.prune

    val transformAcknowledgementSection = (jsValue: JsValue) => {
      val jsObject = jsValue.as[JsObject]
      val displayFeedbackLinkValueOrDefault: JsValue = jsObject.value.getOrElse("displayFeedbackLink", JsTrue)
      jsObject ++ Json.obj("displayFeedbackLink" -> displayFeedbackLinkValueOrDefault)
    }

    val transformAndMoveAcknowledgementSection =
      (__ \ 'destinations \ 'acknowledgementSection).json
        .copyFrom((__ \ 'acknowledgementSection).json.pick.map(transformAcknowledgementSection)) orElse Reads.pure(
        Json.obj())

    val moveDestinations =
      (__ \ 'destinations \ 'destinations).json
        .copyFrom((__ \ 'destinations).json.pick) orElse Reads.pure(Json.obj())

    val moveDeclarationSection =
      (__ \ 'destinations \ 'declarationSection).json
        .copyFrom((__ \ 'declarationSection).json.pick) orElse Reads.pure(Json.obj())

    val pruneAcknowledgementSection = (__ \ 'acknowledgementSection).json.prune

    val pruneDeclarationSection = (__ \ 'declarationSection).json.prune

    val sectionValidations: JsResult[Unit] =
      (
        (jsonValue \ "destinations").toOption,
        (jsonValue \ "printSection").toOption,
        (jsonValue \ "acknowledgementSection").toOption,
        (jsonValue \ "declarationSection").toOption) match {
        case (Some(_), Some(_), _, _)          => onlyOneOfDestinationsAndPrintSection
        case (None, None, _, _)                => onlyOneOfDestinationsAndPrintSection
        case (Some(_), None, None, _)          => mandatoryAcknowledgementForDestinationSection
        case (Some(_), None, _, None)          => mandatoryDeclarationForDestinationSection
        case (None, Some(_), Some(_), _)       => avoidAcknowledgementForPrintSection
        case (None, Some(_), _, Some(_))       => avoidDeclarationForPrintSection
        case (Some(_), None, Some(_), Some(_)) => JsSuccess(())
        case (None, Some(_), None, None)       => JsSuccess(())
      }

    val dmsFormIdValidations: JsResult[Unit] =
      (jsonValue \ "destinations").toOption match {
        case Some(id) =>
          val errorMessages = id
            .as[JsArray]
            .value
            .map { jv =>
              ((jv \ "id").as[String], (jv \ "type").as[String], (jv \ "dmsFormId").toOption.map(_.as[String]))
            }
            .map {
              // format: off
              case (_, "hmrcDms", Some(dmsFormId))
                  if dmsFormId.toString.length > 0 && dmsFormId.toString.length <= 12 => ""
              case (id, "hmrcDms", Some(_))                                           => invalidDmsFormId(id)
              case (id, "hmrcDms", None)                                              => missingDmsFormId(id)
              case _                                                                  => ""
              // format: on
            }
            .filter(_.nonEmpty)

          if (errorMessages.isEmpty)
            JsSuccess(())
          else
            JsError(errorMessages.mkString(" "))

        case None => JsSuccess(())
      }

    sectionValidations andKeep dmsFormIdValidations andKeep jsonValue.transform(
      pruneShowContinueOrDeletePage andThen pruneAcknowledgementSection andThen prunePrintSection andThen pruneDeclarationSection and drmValue and drmShowContinueOrDeletePage and ensureDisplayHMRCLogo and ensureFormCategory and
        ensureLanguages and ensureSummarySection and ensureParentFormSubmissionRefs and destinationsOrPrintSection and transformAndMoveAcknowledgementSection and moveDestinations and moveDeclarationSection reduce)
  }
}
