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

import cats.instances.future._
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import play.api.libs.json.{ JsObject, Json }

import scala.util.Random
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.InvalidState
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.typeclasses.{ FindOne, Insert, Now, Post, Rnd }

import scala.concurrent.ExecutionContext.Implicits.global
import java.time.LocalDateTime

import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

object SubmissionService {

  def getSectionFormFields(
    form: Form,
    formTemplate: FormTemplate
  ): Opt[List[SectionFormField]] = {
    val data: Map[FieldId, FormField] = form.formData.fields.map(field => field.id -> field).toMap

    val formFieldByFieldValue: FieldValue => Opt[List[(FormField, FieldValue)]] = fieldValue => {
      val fieldValueIds: List[FieldId] =
        fieldValue.`type` match {
          case Address(_) => Address.fields(fieldValue.id)
          case Date(_, _, _) => Date.fields(fieldValue.id)
          case FileUpload() => List(fieldValue.id)
          case Text(_, _, _) | Choice(_, _, _, _, _) | Group(_, _, _, _, _, _) => List(fieldValue.id) // TODO - added Group just to compile; remove if possible
          case InformationMessage(_, _) => List(fieldValue.id)
        }

      val formFieldAndFieldValues: List[Opt[(FormField, FieldValue)]] =
        fieldValueIds.map { fieldValueId =>
          data.get(fieldValueId) match {
            case Some(formField) => Right((formField, fieldValue))
            case None => Left(InvalidState(s"No formField for field.id: ${fieldValue.id} found"))
          }
        }

      formFieldAndFieldValues.sequenceU
    }

    val toSectionFormField: Section => Opt[SectionFormField] = section =>
      section.atomicFields(data).flatTraverse(formFieldByFieldValue).map(ff => SectionFormField(section.title, ff))

    val allSections = formTemplate.sections
    val sectionsToSubmit = allSections.filter(section => BooleanExpr.isTrue(section.includeIf.getOrElse(IncludeIf(IsTrue)).expr, data))
    sectionsToSubmit.toList.traverse(toSectionFormField)
  }

  def getSubmissionAndPdf(
    envelopeId: EnvelopeId,
    form: Form,
    sectionFormFields: List[SectionFormField],
    formName: String
  )(
    implicit
    now: Now[LocalDateTime],
    rnd: Rnd[Random]
  ): SubmissionAndPdf = {

    val pdf: Array[Byte] = PdfGenerator.generate(sectionFormFields, formName)

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
      submissionAndPdf  =  getSubmissionAndPdf(envelopeId, form, sectionFormFields, formTemplate.formName)
      _                 <- fromFutureA        (insertSubmission(Json.obj(), submissionAndPdf.submission))
      res               <- FileUploadService.submitEnvelope(submissionAndPdf, formTemplate.dmsSubmission)
    } yield res
    // format: ON
  }
}
