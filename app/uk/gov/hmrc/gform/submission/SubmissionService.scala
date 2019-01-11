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

import cats.instances.either._
import cats.instances.future._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import org.apache.pdfbox.pdmodel.PDDocument
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.email.EmailService
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.form.FormService
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, RepeatingComponentService, SectionHelper }
import uk.gov.hmrc.gform.pdfgenerator.{ HtmlGeneratorService, PdfGeneratorService, XmlGeneratorService }
import uk.gov.hmrc.gform.sharedmodel.Visibility
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.{ ExecutionContext, Future }

class SubmissionService(
  pdfGeneratorService: PdfGeneratorService,
  formService: FormService,
  formTemplateService: FormTemplateService,
  fileUploadService: FileUploadService,
  submissionRepo: SubmissionRepo,
  timeProvider: TimeProvider,
  email: EmailService) {

  def submissionWithoutPdf(formId: FormId, customerId: String, affinityGroup: Option[AffinityGroup])(
    implicit hc: HeaderCarrier): FOpt[Unit] =
    // format: OFF
    for {
      form                <- fromFutureA        (getSignedForm(formId))
      res                 <-                    submission(form, customerId, affinityGroup, getSubmissionAndPdf(form.envelopeId, form, _, _, customerId))
    } yield res
  // format: ON

  def submissionWithPdf(formId: FormId, customerId: String, affinityGroup: Option[AffinityGroup], pdf: String)(
    implicit hc: HeaderCarrier): FOpt[Unit] =
    // format: OFF
    for {
      form                <- fromFutureA        (formService.get(formId))
      res                 <-                    submission(form, customerId, affinityGroup, getSubmissionAndPdfWithPdf(form.envelopeId, form, _, _, customerId, pdf))
    } yield res
  // format: ON

  private def getNoOfAttachments(form: Form, formTemplate: FormTemplate): Int = {
    // TODO two functions are calculating the same thing in different ways! c.f. FileUploadService.SectionFormField.getNumberOfFiles
    val attachmentsIds: List[String] =
      formTemplate.sections.flatMap(_.fields.filter(f => f.`type` == FileUpload())).map(_.id.value)
    val formIds: Seq[String] = form.formData.fields.filterNot(_.value == FileUploadField.noFileUpload).map(_.id.value)
    attachmentsIds.count(ai => formIds.contains(ai))
  }

  //todo install 3rd part part
  private def getSubmissionAndPdf(
    envelopeId: EnvelopeId,
    form: Form,
    sectionFormFields: List[SectionFormField],
    formTemplate: FormTemplate,
    customerId: String)(implicit hc: HeaderCarrier): Future[SubmissionAndPdf] =
    getSubmissionAndPdfWithPdf(
      envelopeId,
      form,
      sectionFormFields,
      formTemplate,
      customerId,
      HtmlGeneratorService.generateDocumentHTML(sectionFormFields, formTemplate.formName, form.formData)
    )

  //todo refactor the two methods into one
  private def getSubmissionAndPdfWithPdf(
    envelopeId: EnvelopeId,
    form: Form,
    sectionFormFields: List[SectionFormField],
    formTemplate: FormTemplate,
    customerId: String,
    pdfHtml: String)(implicit hc: HeaderCarrier): Future[SubmissionAndPdf] =
    pdfGeneratorService.generatePDF(pdfHtml).map { pdf =>
      val pdfSummary = createPdfSummary(pdf)
      val submission = createSubmission(envelopeId, form, customerId, formTemplate)
      val xmlSummary = createXmlSummary(sectionFormFields, formTemplate, submission)

      SubmissionAndPdf(submission = submission, pdfSummary = pdfSummary, xmlSummary = xmlSummary)
    }

  private def createPdfSummary(pdf: Array[Byte]) = {
    val pDDocument: PDDocument = PDDocument.load(pdf)
    try {
      PdfSummary(numberOfPages = pDDocument.getNumberOfPages.toLong, pdfContent = pdf)
    } finally {
      pDDocument.close()
    }
  }

  private def createSubmission(envelopeId: EnvelopeId, form: Form, customerId: String, formTemplate: FormTemplate) =
    Submission(
      submittedDate = timeProvider.localDateTime(),
      submissionRef = SubmissionRef.random,
      envelopeId = envelopeId,
      _id = form._id,
      noOfAttachments = getNoOfAttachments(form, formTemplate),
      dmsMetaData = DmsMetaData(
        formTemplateId = form.formTemplateId,
        customerId //TODO need more secure and safe way of doing this. perhaps moving auth to backend and just pulling value out there.
      )
    )

  private def createXmlSummary(
    sectionFormFields: List[SectionFormField],
    formTemplate: FormTemplate,
    submission: Submission) = {
    val xmlSummary = formTemplate.dmsSubmission.dataXml match {
      case Some(true) =>
        Some(
          XmlGeneratorService.xmlDec + "\n" + XmlGeneratorService.getXml(sectionFormFields, submission.submissionRef))
      case _ => None

    }
    xmlSummary
  }

  private def getSignedForm(formId: FormId)(implicit hc: HeaderCarrier) = formService.get(formId).flatMap {
    case f @ Form(_, _, _, _, _, _, Signed) => Future.successful(f)
    case _                                  => Future.failed(new Exception(s"Form $FormId status is not signed"))
  }

  private def submission(
    form: Form,
    customerId: String,
    affinityGroup: Option[AffinityGroup],
    pdfAccessor: (List[SectionFormField], FormTemplate) => Future[SubmissionAndPdf])(
    implicit hc: HeaderCarrier): FOpt[Unit] =
    // format: OFF
    for {
      formTemplate        <- fromFutureA          (formTemplateService.get(form.formTemplateId))
      sectionFormFields   <- fromOptA             (SubmissionServiceHelper.getSectionFormFields(form, formTemplate, affinityGroup))
      submissionAndPdf    <- fromFutureA          (pdfAccessor(sectionFormFields, formTemplate))
      _                   <-                      submissionRepo.upsert(submissionAndPdf.submission)
      _                   <- fromFutureA          (formService.updateUserData(form._id, UserData(form.formData, form.repeatingGroupStructure, Submitted)))
      numberOfAttachments =                       sectionFormFields.map(_.numberOfFiles).sum
      res                 <- fromFutureA          (fileUploadService.submitEnvelope(submissionAndPdf, formTemplate.dmsSubmission, numberOfAttachments))
      emailAddress        =                       email.getEmailAddress(form)
      _                   <-                      fromFutureA(email.sendEmail(emailAddress, formTemplate.emailTemplateId, getEmailParameterValues(formTemplate, form))(hc, fromLoggingDetails))
    } yield res
  // format: ON

  def submissionDetails(formId: FormId)(implicit ec: ExecutionContext): Future[Submission] =
    submissionRepo.get(formId.value)

  def getEmailParameterValues(formTemplate: FormTemplate, form: Form): Map[String, String] =
//    formTemplate.emailParameters
//      .map { parameter =>
//        form.formData.fields
//          .groupBy(_.value)
//          .find(field => field._2.head.id.toString == parameter)
//          .map(found => found._2.head.value)
//          .getOrElse("")
//
//      }
    formTemplate.emailParameters
      .flatMap(
        parameter =>
          form.formData.fields
            .find(field => field.id.value == parameter)
            .map(f => (f.id.value, f.value)))
      .toMap

}

object SubmissionServiceHelper {

  def getSectionFormFields(
    form: Form,
    formTemplate: FormTemplate,
    affinityGroup: Option[AffinityGroup]): Opt[List[SectionFormField]] = {

    val data: Map[FormComponentId, FormField] = form.formData.fields.map(field => field.id -> field).toMap

    val formFieldByFieldValue: FormComponent => Opt[(List[FormField], FormComponent)] = fieldValue => {
      val fieldValueIds: List[FormComponentId] =
        fieldValue.`type` match {
          case Address(_)    => Address.fields(fieldValue.id)
          case Date(_, _, _) => Date.fields(fieldValue.id)
          case FileUpload()  => List(fieldValue.id)
          case UkSortCode(_) => UkSortCode.fields(fieldValue.id)
          case Text(_, _, _) | TextArea(_, _, _) | Choice(_, _, _, _, _) | Group(_, _, _, _, _, _) =>
            List(fieldValue.id)
          case InformationMessage(_, _) => Nil
        }

      val formFieldAndFieldValues: Opt[List[FormField]] = {
        fieldValueIds
          .map { fieldValueId =>
            data.get(fieldValueId) match {
              case Some(formField) => Right(formField)
              case None            => Left(UnexpectedState(s"No formField for field.id: ${fieldValue.id} found"))
            }
          }
          .partition(_.isLeft) match {
          case (Nil, list) => Right(for (Right(formField) <- list) yield formField)
          case (invalidStates, _) =>
            Left(UnexpectedState((for (Left(invalidState) <- invalidStates) yield invalidState.error).mkString(", ")))
        }
      }

      formFieldAndFieldValues match {
        case Right(list)        => Right((list, fieldValue))
        case Left(invalidState) => Left(invalidState)
      }
    }

    val toSectionFormField: BaseSection => Opt[SectionFormField] = section =>
      SectionHelper
        .atomicFields(section, data)
        .traverse(formFieldByFieldValue)
        .map(ff => SectionFormField(section.shortName.getOrElse(section.title), ff))

    val allSections = RepeatingComponentService.getAllSections(form, formTemplate)

    val visibility = Visibility(allSections, data.mapValues(_.value :: Nil), affinityGroup)
    val filteredSection = allSections.filter(visibility.isVisible)

    val sectionsToSubmit = filteredSection :+ formTemplate.declarationSection
    sectionsToSubmit.traverse(toSectionFormField)
  }

}
