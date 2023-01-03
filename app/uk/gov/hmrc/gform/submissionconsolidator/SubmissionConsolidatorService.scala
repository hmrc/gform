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

package uk.gov.hmrc.gform.submissionconsolidator

import java.time.format.DateTimeFormatter

import cats.data.EitherT
import cats.implicits._
import play.api.libs.json.Json
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, HandlebarsTemplateProcessorModel, TemplateType }
import uk.gov.hmrc.gform.submission.destinations.DestinationSubmissionInfo
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsModelTree, HandlebarsTemplateProcessor }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

trait SubmissionConsolidatorAlgebra[F[_]] {
  def submit(
    destination: Destination.SubmissionConsolidator,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    formData: Option[FormData]
  )(implicit headerCarrier: HeaderCarrier): F[Unit]
}

class SubmissionConsolidatorService(
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor,
  submissionConsolidatorConnector: SubmissionConsolidatorConnector,
  formService: FormAlgebra[FOpt]
)(implicit ec: ExecutionContext)
    extends SubmissionConsolidatorAlgebra[FOpt] {

  private val DATE_TIME_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")

  def submit(
    destination: Destination.SubmissionConsolidator,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    formData: Option[FormData]
  )(implicit headerCarrier: HeaderCarrier): FOpt[Unit] =
    for {
      scForm         <- buildSCForm(destination, submissionInfo, formData, accumulatedModel, modelTree)
      sendFormResult <- EitherT(submissionConsolidatorConnector.sendForm(scForm)).leftMap(UnexpectedState)
      _              <- formService.updateFormStatus(submissionInfo.formId, Submitted)
    } yield sendFormResult

  private def buildSCForm(
    destination: Destination.SubmissionConsolidator,
    submissionInfo: DestinationSubmissionInfo,
    formData: Option[FormData],
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree
  ): EitherT[Future, UnexpectedState, SCForm] =
    for {
      scFormFields <- buildSCFormFields(destination, formData, accumulatedModel, modelTree)
    } yield SCForm(
      submissionInfo.submission.submissionRef.value,
      destination.projectId.id,
      submissionInfo.submission.dmsMetaData.formTemplateId.value,
      submissionInfo.customerId,
      submissionInfo.submission.submittedDate.format(DATE_TIME_FORMAT),
      scFormFields.filter(_.value.trim.nonEmpty)
    )

  private def buildSCFormFields(
    destination: Destination.SubmissionConsolidator,
    formData: Option[FormData],
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree
  ): EitherT[Future, UnexpectedState, Seq[SCFormField]] =
    destination.formData
      .map(buildSCFormFieldsFromTemplate(_, accumulatedModel, modelTree))
      .orElse(formData.map(buildSCFormFieldsFromForm))
      .getOrElse(EitherT.pure(List.empty))

  private def buildSCFormFieldsFromForm(formData: FormData): EitherT[Future, UnexpectedState, Seq[SCFormField]] =
    EitherT {
      Future.successful {
        Right(formData.fields.map { case FormField(FormComponentId(id), value) =>
          SCFormField(id, value)
        }): Either[UnexpectedState, Seq[SCFormField]]
      }
    }

  private def buildSCFormFieldsFromTemplate(
    formDataTemplate: String,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree
  ): EitherT[Future, UnexpectedState, Seq[SCFormField]] = EitherT {
    Future.successful {
      Try(
        Json
          .parse(
            handlebarsTemplateProcessor(
              formDataTemplate,
              accumulatedModel,
              FocussedHandlebarsModelTree(modelTree),
              TemplateType.JSON
            )
          )
          .as[Seq[SCFormField]]
      ) match {
        case Success(scFormFields) => Right(scFormFields)
        case Failure(exception)    => Left(UnexpectedState(exception.getMessage))
      }
    }
  }
}
