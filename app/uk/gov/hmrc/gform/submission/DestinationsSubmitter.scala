/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission

import java.nio.charset.StandardCharsets
import java.util.Base64

import cats.Monad
import cats.instances.int._
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import play.api.Logger
import uk.gov.hmrc.gform.fileupload.{ FileDownloadAlgebra, UploadedFile }
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.http.HeaderCarrier

trait DestinationsSubmitterAlgebra[M[_]] {
  def send(submissionInfo: DestinationSubmissionInfo, formTemplate: FormTemplate, formAlgebra: FormAlgebra[M])(
    implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]]

  def submitToList(
    destinations: Destinations.DestinationList,
    submissionInfo: DestinationSubmissionInfo,
    handlebarsModel: HandlebarsTemplateProcessorModel,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]]
}

class DestinationsSubmitter[M[_]](
  destinationSubmitter: DestinationSubmitter[M],
  fileDownloadAlgebra: Option[FileDownloadAlgebra[M]])(implicit monad: Monad[M])
    extends DestinationsSubmitterAlgebra[M] {

  override def send(submissionInfo: DestinationSubmissionInfo, formTemplate: FormTemplate, formAlgebra: FormAlgebra[M])(
    implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    formTemplate.destinations match {
      case dms: Destinations.DmsSubmission =>
        destinationSubmitter.submitToDms(submissionInfo, dms).map(_ => None)
      case list: Destinations.DestinationList =>
        submitToList(list, submissionInfo, formAlgebra, formTemplate)
    }

  def submitToList(
    destinations: Destinations.DestinationList,
    submissionInfo: DestinationSubmissionInfo,
    formAlgebra: FormAlgebra[M],
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    for {
      form  <- formAlgebra.get(submissionInfo.formId)
      files <- uploadedFiles(form.envelopeId)
      result <- submitToList(
                 destinations,
                 submissionInfo,
                 DestinationsSubmitter.createHandlebarsTemplateProcessorModel(submissionInfo, form, files),
                 formTemplate)
    } yield result

  def submitToList(
    destinations: Destinations.DestinationList,
    submissionInfo: DestinationSubmissionInfo,
    handlebarsModel: HandlebarsTemplateProcessorModel,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] = {
    case class TailRecParameter(
      remainingDestinations: List[Destination],
      accumulatedModel: HandlebarsTemplateProcessorModel)

    TailRecParameter(destinations.destinations.toList, handlebarsModel).tailRecM {
      case TailRecParameter(Nil, _) => Option.empty[HandlebarsDestinationResponse].asRight[TailRecParameter].pure[M]
      case TailRecParameter(head :: rest, model) =>
        destinationSubmitter
          .submitIfIncludeIf(head, submissionInfo, model, this, formTemplate)
          .map(submitterResult =>
            TailRecParameter(rest, submitterResult.fold(model) { HandlebarsTemplateProcessorModel(_) + model }).asLeft)
    }
  }

  private def uploadedFiles(envelopedId: EnvelopeId)(implicit hc: HeaderCarrier): M[Option[List[UploadedFile]]] =
    fileDownloadAlgebra.traverse { _.allUploadedFiles(envelopedId) }
}

object DestinationsSubmitter {
  def createHandlebarsTemplateProcessorModel(
    submissionInfo: DestinationSubmissionInfo,
    form: Form,
    files: Option[List[UploadedFile]]): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel.formId(form._id) +
      HandlebarsTemplateProcessorModel(submissionInfo.submissionData.structuredFormData) +
      HandlebarsTemplateProcessorModel.hmrcTaxPeriods(form) +
      HandlebarsTemplateProcessorModel.rosmRegistration(form) +
      HandlebarsTemplateProcessorModel(submissionInfo.submissionData.variables) +
      HandlebarsTemplateProcessorModel(form.status) +
      HandlebarsTemplateProcessorModel.summaryHtml(submissionInfo.submissionData.pdfData) +
      HandlebarsTemplateProcessorModel.submissionReference(submissionInfo.submission.submissionRef.value) +
      uploadedFilesModel(files)

  private def uploadedFilesModel(oFiles: Option[List[UploadedFile]]) =
    oFiles.fold(HandlebarsTemplateProcessorModel.empty) { files =>
      HandlebarsTemplateProcessorModel(
        "uploadedFiles" -> JsonNodes.arrayNode(files.map(asJson))
      )
    }

  private def asJson(file: UploadedFile) = {
    val name = file.file.fileName
    val extension = fileExtension(file.file.fileName)
    val data = new String(Base64.getEncoder.encode(file.data.toArray), StandardCharsets.UTF_8)

    Logger.info(s"Creating node for : $name with extension $extension and data $data")

    JsonNodes.objectNode(
      Map(
        "name"      -> JsonNodes.textNode(name),
        "extension" -> JsonNodes.textNode(extension),
        "data"      -> JsonNodes.textNode(data)
      ))
  }

  private def fileExtension(filename: String): String = {
    val dotIndex = filename.indexOf(".")
    if (dotIndex === -1) "" else filename.substring(dotIndex + 1)
  }
}
