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

package uk.gov.hmrc.gform.submission

import cats.instances.either._
import cats.instances.future._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.fileUpload.FileUploadService
import uk.gov.hmrc.gform.form.FormService
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, RepeatingComponentService, SectionHelper }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.pdfgenerator.{ HtmlGeneratorService, PdfGeneratorService }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormField, FormId }
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubmissionService(
    pdfGeneratorService: PdfGeneratorService,
    formService: FormService,
    formTemplateService: FormTemplateService,
    fileUploadService: FileUploadService,
    submissionRepo: SubmissionRepo,
    timeProvider: TimeProvider

) {

  def getSubmissionAndPdf(
    envelopeId: EnvelopeId,
    form: Form,
    sectionFormFields: List[SectionFormField],
    formName: String
  )(implicit hc: HeaderCarrier): Future[SubmissionAndPdf] = {

    val html = HtmlGeneratorService.generateDocumentHTML(sectionFormFields, formName)

    pdfGeneratorService.generatePDF(html).map { pdf =>

      /*
      val path = java.nio.file.Paths.get("confirmation.pdf")
      val out = java.nio.file.Files.newOutputStream(path)
      out.write(pdf)
      out.close()
      */

      val pdfSummary = PdfSummary(
        numberOfPages = 1L,
        pdfContent = pdf
      )
      val submission = Submission(
        submittedDate = timeProvider.localDateTime(),
        submissionRef = SubmissionRef.random,
        envelopeId = envelopeId,
        _id = form._id,
        dmsMetaData = DmsMetaData(
          formTypeId = form.formTemplateId
        )
      )

      SubmissionAndPdf(
        submission = submission,
        pdfSummary = pdfSummary
      )
    }
  }

  def submission(formId: FormId)(implicit hc: HeaderCarrier): FOpt[Unit] = {

    // format: OFF
    for {
      form              <- fromFutureA        (formService.get(formId))
      formTemplate      <- fromFutureA        (formTemplateService.get(form.formTemplateId))
      sectionFormFields <- fromOptA           (SubmissionServiceHelper.getSectionFormFields(form, formTemplate))
      submissionAndPdf  <- fromFutureA        (getSubmissionAndPdf(form.envelopeId, form, sectionFormFields, formTemplate.formName))
      _                 <-                     submissionRepo.upsert(submissionAndPdf.submission)
      res               <- fromFutureA        (fileUploadService.submitEnvelope(submissionAndPdf, formTemplate.dmsSubmission))
    } yield res
    // format: ON
  }
}

object SubmissionServiceHelper {

  def getSectionFormFields(
    form: Form,
    formTemplate: FormTemplate
  ): Opt[List[SectionFormField]] = {

    val data: Map[FieldId, FormField] = form.formData.fields.map(field => field.id -> field).toMap

    val formFieldByFieldValue: FieldValue => Opt[(List[FormField], FieldValue)] = fieldValue => {
      val fieldValueIds: List[FieldId] =
        fieldValue.`type` match {
          case Address(_) => Address.fields(fieldValue.id)
          case Date(_, _, _) => Date.fields(fieldValue.id)
          case FileUpload() => List(fieldValue.id)
          case Text(_, _, _) | Choice(_, _, _, _, _) | Group(_, _, _, _, _, _) => List(fieldValue.id)
          case InformationMessage(_, _) => List(fieldValue.id)
        }

      val formFieldAndFieldValues: Opt[List[FormField]] = {
        fieldValueIds.map { fieldValueId =>
          data.get(fieldValueId) match {
            case Some(formField) => Right(formField)
            case None => Left(UnexpectedState(s"No formField for field.id: ${fieldValue.id} found"))
          }
        }.partition(_.isLeft) match {
          case (Nil, list) => Right(for (Right(formField) <- list) yield formField)
          case (invalidStates, _) => Left(UnexpectedState((for (Left(invalidState) <- invalidStates) yield invalidState.error).mkString(", ")))
        }
      }

      formFieldAndFieldValues match {
        case Right(list) => Right((list, fieldValue))
        case Left(invalidState) => Left(invalidState)
      }
    }

    val toSectionFormField: Section => Opt[SectionFormField] = section =>
      SectionHelper.atomicFields(section, data).traverse(formFieldByFieldValue).map(ff => SectionFormField(section.shortName.getOrElse(section.title), ff))

    val allSections = RepeatingComponentService.getAllSections(form, formTemplate)
    val sectionsToSubmit = allSections.filter(section => BooleanExpr.isTrue(section.includeIf.getOrElse(IncludeIf(IsTrue)).expr, data))
    sectionsToSubmit.traverse(toSectionFormField)
  }

}