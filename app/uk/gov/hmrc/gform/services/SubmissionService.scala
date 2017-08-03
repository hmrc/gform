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

package uk.gov.hmrc.gform.services

import java.time.LocalDateTime

import cats.instances.either._
import cats.instances.future._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import play.api.libs.json.{ JsObject, Json }
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.{ InvalidState, UnexpectedState }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.typeclasses._
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object SubmissionService extends SubmissionService {
  lazy val pdfGenerator = PDFGeneratorService
  lazy val htmlGenerator = HtmlGeneratorService
}

trait SubmissionService {

  def pdfGenerator: PDFGeneratorService
  def htmlGenerator: HtmlGeneratorService

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
            case None => Left(InvalidState(s"No formField for field.id: ${fieldValue.id} found"))
          }
        }.partition(_.isLeft) match {
          case (Nil, list) => Right(for (Right(formField) <- list) yield formField)
          case (invalidStates, _) => Left(InvalidState((for (Left(invalidState) <- invalidStates) yield invalidState).mkString(", ")))
        }
      }

      formFieldAndFieldValues match {
        case Right(list) => Right((list, fieldValue))
        case Left(invalidState) => Left(invalidState)
      }
    }

    val toSectionFormField: Section => Opt[SectionFormField] = section =>
      section.atomicFields(data).traverse(formFieldByFieldValue).map(ff => SectionFormField(section.shortName.getOrElse(section.title), ff))

    val allSections = RepeatingComponentService.getAllSections(form, formTemplate)
    val sectionsToSubmit = allSections.filter(section => BooleanExpr.isTrue(section.includeIf.getOrElse(IncludeIf(IsTrue)).expr, data))
    sectionsToSubmit.traverse(toSectionFormField)
  }

  def getSubmissionAndPdf(
    envelopeId: EnvelopeId,
    form: Form,
    sectionFormFields: List[SectionFormField],
    formName: String
  )(
    implicit
    now: Now[LocalDateTime],
    rnd: Rnd[Random],
    hc: HeaderCarrier
  ): Future[SubmissionAndPdf] = {

    val html = htmlGenerator.generateDocumentHTML(sectionFormFields, formName)

    pdfGenerator.generatePDF(html).map { pdf =>
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
        submittedDate = now(),
        submissionRef = SubmissionRef.random,
        envelopeId = envelopeId,
        formId = form._id,
        dmsMetaData = DmsMetaData(
          formTypeId = form.formData.formTypeId
        )
      )

      SubmissionAndPdf(
        submission = submission,
        pdfSummary = pdfSummary
      )
    }
  }

  def submission(
    formId: FormId
  )(
    implicit
    findOneForm: FindOne[Form],
    findOneFormTemplate: FindOne[FormTemplate],
    insertSubmission: Insert[Submission],
    uploadFile: Post[UploadFile, HttpResponse],
    hc: HeaderCarrier,
    routeEnvelope: Post[RouteEnvelopeRequest, HttpResponse],
    now: Now[LocalDateTime],
    rnd: Rnd[Random]
  ): ServiceResponse[String] = {

    val templateSelector: Form => JsObject = form => Json.obj(
      "formTypeId" -> form.formData.formTypeId
    )
    // format: OFF
    for {
      form              <- FormService.get(formId)
      formTemplate      <- fromFutureOptionA  (findOneFormTemplate(templateSelector(form)))(InvalidState(s"FormTemplate $templateSelector not found"))
      envelopeId        = form.envelopeId
      sectionFormFields <- fromOptA           (getSectionFormFields(form, formTemplate))
      submissionAndPdf  <- fromFutureA        (getSubmissionAndPdf(envelopeId, form, sectionFormFields, formTemplate.formName))
      _                 <- fromFutureA        (insertSubmission(Json.obj(), submissionAndPdf.submission))
      res               <- FileUploadService.submitEnvelope(submissionAndPdf, formTemplate.dmsSubmission)
    } yield res
    // format: ON
  }
}
