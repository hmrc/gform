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

package uk.gov.hmrc.gform.submission.destinations

import java.nio.charset.StandardCharsets
import java.util.Base64

import cats.instances.int._
import cats.syntax.eq._
import com.fasterxml.jackson.databind.JsonNode
import uk.gov.hmrc.gform.fileupload.UploadedFile
import uk.gov.hmrc.gform.form.BundledFormTreeNode
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper.formatDate
import uk.gov.hmrc.gform.sharedmodel.{ FrontEndSubmissionVariables, NotChecked, ObligationDetail, PdfHtml, RetrievedObligations, SubmissionRef, TaxResponse }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.JsonNodes.{ arrayNode, numberNode, objectNode, parseJson, textNode }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HandlebarsDestinationResponse, HandlebarsTemplateProcessorModel, JsonNodes, JsonStructuredFormDataBuilder }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.Tree
import uk.gov.hmrc.http.HeaderCarrier

trait DestinationsProcessorModelAlgebra[M[_]] {
  def create(
    form: Form,
    frontEndSubmissionVariables: FrontEndSubmissionVariables,
    pdfData: PdfHtml,
    instructionPdfHtml: Option[PdfHtml],
    structuredFormData: StructuredFormValue.ObjectStructure)(
    implicit hc: HeaderCarrier): M[HandlebarsTemplateProcessorModel]
}

object DestinationsProcessorModelAlgebra {
  def createModel(
    frontEndSubmissionVariables: FrontEndSubmissionVariables,
    pdfData: PdfHtml,
    instructionPdfData: Option[PdfHtml],
    structuredFormData: StructuredFormValue.ObjectStructure,
    form: Form,
    files: Option[List[UploadedFile]]): HandlebarsTemplateProcessorModel =
    createFormId(form._id) +
      createStructuredFormData(structuredFormData) +
      createHmrcTaxPeriods(form) +
      createRosmRegistration(form) +
      createFrontEndSubmissionVariables(frontEndSubmissionVariables) +
      createFormStatus(form.status) +
      createPdfHtml(pdfData) +
      createInstructionPdfHtml(instructionPdfData) +
      createSubmissionReference(SubmissionRef(form.envelopeId).value) +
      createUploadedFiles(files) +
      createCaseworker(form.thirdPartyData.reviewData.flatMap(_.get("caseworker")))

  def createDestinationResponse(result: HandlebarsDestinationResponse): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(
      Map(
        result.id.id -> objectNode(
          Map("status" -> numberNode(result.status), "json" -> parseJson(result.json.toString)))))

  def createBundledFormTree(tree: Tree[BundledFormTreeNode]): HandlebarsTemplateProcessorModel = {
    def submissionRefString(tree: BundledFormTreeNode) = tree.submissionRef.value

    def childForest(forest: List[Tree[BundledFormTreeNode]]) = arrayNode(forest.map(treeNode))

    def childGroups(tree: Tree[BundledFormTreeNode]) =
      tree.children.groupBy(_.value.formTemplateId).map {
        case (templateId: FormTemplateId, forest: Seq[Tree[BundledFormTreeNode]]) =>
          (templateId.value, childForest(forest))
      }

    def treeNode(tree: Tree[BundledFormTreeNode]): JsonNode =
      objectNode(Map(
        "formId"        -> textNode(tree.value.formId.value),
        "submissionRef" -> textNode(submissionRefString(tree.value))) ++
        childGroups(tree))

    def submissionRefs: JsonNode = {
      def extractSubmissionRefs(t: Tree[BundledFormTreeNode]): Set[String] =
        t.fold(Set.empty[String]) { _ + submissionRefString(_) }

      arrayNode(extractSubmissionRefs(tree).map(textNode).toList)
    }

    HandlebarsTemplateProcessorModel(
      "formBundle" -> objectNode(
        "tree"           -> treeNode(tree),
        "submissionRefs" -> submissionRefs
      ))
  }

  private def asJson(file: UploadedFile) = {
    val name = file.file.fileName
    val extension = fileExtension(file.file.fileName)
    val data = new String(Base64.getEncoder.encode(file.data.toArray), StandardCharsets.UTF_8)

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

  private def createCaseworker(maybeCaseworker: Option[String]): HandlebarsTemplateProcessorModel =
    maybeCaseworker
      .map(caseworker => HandlebarsTemplateProcessorModel("caseworker" -> textNode(caseworker)))
      .getOrElse(HandlebarsTemplateProcessorModel.empty)

  private def createUploadedFiles(oFiles: Option[List[UploadedFile]]): HandlebarsTemplateProcessorModel =
    oFiles.fold(HandlebarsTemplateProcessorModel.empty) { files =>
      HandlebarsTemplateProcessorModel(
        "uploadedFiles" -> JsonNodes.arrayNode(files.map(asJson))
      )
    }

  private def createStructuredFormData(
    structuredData: StructuredFormValue.ObjectStructure): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(JsonStructuredFormDataBuilder(structuredData))

  def createFormId(formId: FormId): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(Map("formId" -> textNode(formId.value)))

  private def createFrontEndSubmissionVariables(
    variables: FrontEndSubmissionVariables): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(variables.value.toString)

  def createFormStatus(status: FormStatus): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(Map("formStatus" -> textNode(status.toString)))

  private def createPdfHtml(html: PdfHtml): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel("summaryHtml" -> textNode(html.html))

  private def createInstructionPdfHtml(html: Option[PdfHtml]): HandlebarsTemplateProcessorModel =
    html.fold(HandlebarsTemplateProcessorModel.empty) { html =>
      HandlebarsTemplateProcessorModel("instructionHtml" -> textNode(html.html))
    }

  private def createSubmissionReference(submissionReference: String): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel("submissionReference" -> textNode(submissionReference))

  def createHmrcTaxPeriods(form: Form): HandlebarsTemplateProcessorModel = {

    val lookup: Map[FormComponentId, String] = form.formData.fields.map(fd => fd.id -> fd.value).toMap

    def mkMap(od: ObligationDetail): Map[String, String] = Map(
      "periodKey"  -> od.periodKey,
      "periodFrom" -> formatDate(od.inboundCorrespondenceFromDate),
      "periodTo"   -> formatDate(od.inboundCorrespondenceToDate)
    )

    def toJsonNode(taxResponse: TaxResponse) = {
      val fcId = taxResponse.id.recalculatedTaxPeriodKey.fcId
      for {
        periodKey <- lookup.get(fcId)
        obligationDetail <- form.thirdPartyData.obligations
                             .findByPeriodKey(taxResponse.id.recalculatedTaxPeriodKey.hmrcTaxPeriod, periodKey)
      } yield Map(fcId.value -> objectNode(mkMap(obligationDetail).mapValues(textNode)))
    }

    val jsonNodes: Map[String, JsonNode] =
      form.thirdPartyData.obligations match {
        case NotChecked => Map.empty
        case RetrievedObligations(taxResponses) =>
          taxResponses.map(toJsonNode).toList.flatten.foldLeft(Map.empty[String, JsonNode])(_ ++ _)
      }

    HandlebarsTemplateProcessorModel(objectNode(jsonNodes))
  }

  private def createRosmRegistration(form: Form): HandlebarsTemplateProcessorModel = {
    val f = form.thirdPartyData.desRegistrationResponse.fold("") _

    HandlebarsTemplateProcessorModel(
      "hmrcRosmRegistrationCheck" -> objectNode(Map(
        "safeId"           -> f(_.safeId),
        "organisationName" -> f(_.orgOrInd.getOrganisationName),
        "organisationType" -> f(_.orgOrInd.getOrganisationType),
        "isAGroup"         -> f(_.orgOrInd.getIsAGroup)
      ).mapValues(textNode)))
  }
}
