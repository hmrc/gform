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

import cats.Monad
import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import play.api.libs.json.JsObject
import uk.gov.hmrc.gform.form.{ BundledFormTreeNode, FormAlgebra }
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.repo.RepoAlgebra
import uk.gov.hmrc.gform.sharedmodel.{ BundledFormSubmissionData, FrontEndSubmissionVariables, PdfHtml }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.submission.destinations.DestinationsProcessorModelAlgebra
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsModelTree, HandlebarsModelTreeNode }
import uk.gov.hmrc.http.HeaderCarrier

class FormBundleSubmissionService[F[_]](
  formAlgebra: FormAlgebra[F],
  formTemplateAlgebra: FormTemplateAlgebra[F],
  destinationProcessorModelAlgebra: DestinationsProcessorModelAlgebra[F],
  destinationsSubmitterAlgebra: DestinationsSubmitterAlgebra[F],
  submissionRepoAlgebra: RepoAlgebra[Submission, F],
  formTreeAlgebra: FormTreeAlgebra[F],
  pdfSummaryAlgebra: PdfSummaryAlgebra[F],
  destinationAuditAlgebra: DestinationAuditAlgebra[F])(implicit M: Monad[F]) {

  def forceUpdateFormStatus(formId: FormId, status: FormStatus)(implicit hc: HeaderCarrier): F[Unit] =
    for {
      _    <- formAlgebra.forceUpdateFormStatus(formId, status)
      form <- formAlgebra.get(formId)
      _    <- destinationAuditAlgebra.auditForcedFormStatusChange(form)
    } yield ()

  def formTree(rootFormId: FormId)(implicit hc: HeaderCarrier): F[Tree[BundledFormTreeNode]] =
    formTreeAlgebra.getFormTree(rootFormId)

  def submitFormBundleAfterReview(rootFormId: FormId, submissionData: NonEmptyList[BundledFormSubmissionData])(
    implicit hc: HeaderCarrier): F[Unit] =
    for {
      _          <- formAlgebra.updateFormStatus(rootFormId, Submitting)
      submission <- submissionRepoAlgebra.get(rootFormId.value)
      submissionInfo = DestinationSubmissionInfo("", None, submission)
      modelTree <- createModelTree(rootFormId, submissionData)
      _         <- destinationsSubmitterAlgebra.send(submissionInfo, modelTree)
    } yield ()

  private def createModelTree(rootFormId: FormId, submissionData: NonEmptyList[BundledFormSubmissionData])(
    implicit hc: HeaderCarrier): F[HandlebarsModelTree] =
    for {
      formTree      <- formTreeAlgebra.getFormTree(rootFormId)
      formsById     <- buildMap(formTree)(_.formId)(formAlgebra.get)
      templatesById <- buildMap(formTree)(_.formTemplateId)(formTemplateAlgebra.get)
      submissionDataByFormId = submissionData.map(d => (d.formId, d)).toList.toMap
      pdfHtmlByFormId <- buildMap(formTree)(_.formId)(pdfSummaryAlgebra.getLatestPdfHtml)
      processorModelByFormId <- buildProcessorModelByFormId(
                                 formTree,
                                 formsById,
                                 pdfHtmlByFormId,
                                 submissionDataByFormId)
    } yield
      formTree.map { formTreeNode =>
        HandlebarsModelTreeNode(
          formTreeNode.submissionRef,
          templatesById(formTreeNode.formTemplateId),
          processorModelByFormId(formTreeNode.formId),
          pdfHtmlByFormId(formTreeNode.formId),
          submissionDataByFormId(formTreeNode.formId).structuredFormData
        )
      }

  private def buildProcessorModelByFormId(
    formTree: Tree[BundledFormTreeNode],
    formsById: Map[FormId, Form],
    pdfHtmlByFormId: Map[FormId, PdfHtml],
    submissionDataByFormId: Map[FormId, BundledFormSubmissionData])(implicit hc: HeaderCarrier) =
    buildMap(formTree)(_.formId)(id =>
      for {
        baseModel <- destinationProcessorModelAlgebra
                      .create(
                        formsById(id),
                        FrontEndSubmissionVariables(JsObject(Nil)),
                        pdfHtmlByFormId(id),
                        submissionDataByFormId(id).structuredFormData)
        caseworkerUserName = formTree.find(_.formId === id).flatMap(_.caseworkerUsername).getOrElse("")
        reviewModel = DestinationsProcessorModelAlgebra.createCaseworker(caseworkerUserName)
        treeModel = DestinationsProcessorModelAlgebra.createBundledFormTree(formTree)
      } yield baseModel + reviewModel + treeModel)

  private def buildMap[K, V](tree: Tree[BundledFormTreeNode])(key: BundledFormTreeNode => K)(
    value: K => F[V]): F[Map[K, V]] = {
    val keys = tree
      .fold(Set.empty[K]) { (acc, n) =>
        acc + key(n)
      }
      .toList
    keys.traverse(value).map(values => keys.zip(values).toMap)
  }
}
