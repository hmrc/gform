/*
 * Copyright 2021 HM Revenue & Customs
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
  verifyAndSave: FormTemplate => ExprSubstitutions => BooleanExprSubstitutions => FOpt[Unit],
  save: FormTemplateRaw => FOpt[Unit]
)(implicit ec: ExecutionContext) {

  val futureInterpreter = new RequestHandlerAlg[FOpt] {
    override def handleRequest(templateRaw: FormTemplateRaw): FOpt[Unit] = {

      val expressionsContextOpt: Opt[ExprSubstitutions] = ExprSubstitutions.from(templateRaw)

      val booleanExpressionsContextOpt: Opt[BooleanExprSubstitutions] =
        BooleanExprSubstitutions.from(templateRaw)

      val formTemplateOpt: Opt[FormTemplate] = FormTemplate
        .transformAndReads(templateRaw.value)
        .fold(errors => UnexpectedState(errors.toString()).asLeft, valid => valid.asRight)

      val formTemplateWithSubstitutions = for {
        expressionsContext        <- expressionsContextOpt
        booleanExpressionsContext <- booleanExpressionsContextOpt
        formTemplate              <- formTemplateOpt
      } yield (formTemplate, expressionsContext, booleanExpressionsContext)

      processAndPersistTemplate(formTemplateWithSubstitutions, templateRaw.lowerCaseId)
    }
  }

  private def processAndPersistTemplate(
    formTemplateOpt: Opt[(FormTemplate, ExprSubstitutions, BooleanExprSubstitutions)],
    templateRaw: FormTemplateRaw
  ): FOpt[Unit] =
    for {
      ft <- fromOptA(formTemplateOpt)
      _  <- verifyAndSave(ft._1)(ft._2)(ft._3)
      _  <- save(templateRaw)
    } yield ()
}

object FormTemplatesControllerRequestHandler {

  val onlyOneOfDestinationsAndPrintSection = JsError(
    """One and only one of FormTemplate.{destinations, printSection} must be defined."""
  )

  val avoidAcknowledgementForPrintSection = JsError("""Avoid acknowledgement section in case of print section.""")

  val avoidDeclarationForPrintSection = JsError("""Avoid declaration section in case of print section.""")

  val mandatoryAcknowledgementForDestinationSection = JsError(
    """Acknowledgement section is mandatory in case of destination section."""
  )

  val mandatoryDeclarationForDestinationSection = JsError(
    """Declaration section is mandatory in case of destination section."""
  )

  val invalidDmsFormId = (id: String) =>
    s"For Destination $id, dmsFormId should have minimum 1 and maximum 12 characters."

  val missingDmsFormId = (id: String) => s"For Destination $id, dmsFormId is missing."

  private val noTemplateId: Reads[JsObject] =
    Reads.failed("Template field _id must be provided and it must be a String")

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

    val ensureOriginalId = (__ \ 'originalId).json.copyFrom((__ \ '_id).json.pick) orElse noTemplateId

    val lowerCaseId: Reads[JsObject] = (__ \ '_id).json.copyFrom(
      (__ \ '_id).json
        .pick[JsString]
        .map(jsString => JsString(jsString.value.toLowerCase))
        .widen
    ) orElse noTemplateId

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
        Json.obj()
      )

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
        (jsonValue \ "declarationSection").toOption
      ) match {
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
              ((jv \ "id").as[String], (jv \ "type").as[String], (jv \ "dmsFormId").asOpt[String].map(_.trim))
            }
            .flatMap {
              // format: off
              case (_, "hmrcDms", Some(dmsFormId))
                  if dmsFormId.length > 0 && dmsFormId.length <= 12  => None
              case (id, "hmrcDms", Some(_))                          => Some(invalidDmsFormId(id))
              case (id, "hmrcDms", None)                             => Some(missingDmsFormId(id))
              case _                                                 => None
              // format: on
            }

          if (errorMessages.isEmpty)
            JsSuccess(())
          else
            JsError(errorMessages.mkString(" "))

        case None => JsSuccess(())
      }

    sectionValidations andKeep dmsFormIdValidations andKeep jsonValue.transform(
      pruneShowContinueOrDeletePage andThen
        pruneAcknowledgementSection andThen
        prunePrintSection andThen
        pruneDeclarationSection and
        drmValue and
        drmShowContinueOrDeletePage and
        ensureOriginalId and
        lowerCaseId and
        ensureDisplayHMRCLogo and
        ensureFormCategory and
        ensureLanguages and
        ensureSummarySection and
        ensureParentFormSubmissionRefs and
        destinationsOrPrintSection and
        transformAndMoveAcknowledgementSection and
        moveDestinations and
        moveDeclarationSection reduce
    )
  }
}
