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

package uk.gov.hmrc.gform.submission

import cats.MonadError
import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._
import cats.syntax.traverse._
import org.slf4j.LoggerFactory
import play.api.libs.json.JsObject
import uk.gov.hmrc.gform.form.{ BundledFormTreeNode, FormAlgebra }
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.repo.RepoAlgebra
import uk.gov.hmrc.gform.sharedmodel.{ BundledFormSubmissionData, FrontEndSubmissionDesIncludeIfEval, FrontEndSubmissionVariables, LangADT, PdfHtml, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.submission.destinations.{ DestinationAuditAlgebra, DestinationSubmissionInfo, DestinationsProcessorModelAlgebra, DestinationsSubmitterAlgebra, FormTreeAlgebra, PdfSummaryAlgebra }
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
  destinationAuditAlgebra: DestinationAuditAlgebra[F]
)(implicit M: MonadError[F, String]) {
  private val logger = LoggerFactory.getLogger(getClass)

  def forceUpdateFormStatus(formId: FormId, status: FormStatus)(implicit hc: HeaderCarrier): F[Unit] =
    for {
      _    <- formAlgebra.forceUpdateFormStatus(formId, status)
      form <- formAlgebra.get(formId)
      _    <- destinationAuditAlgebra.auditForcedFormStatusChange(form)
    } yield ()

  def formTree(rootFormId: FormIdData)(implicit hc: HeaderCarrier): F[Tree[BundledFormTreeNode]] =
    for {
      tree         <- formTreeAlgebra.getFormTree(rootFormId)
      validFormIds <- workOutValidFormIds(tree)
      filtered     <- filterForValidFormIds(tree, validFormIds)
    } yield filtered

  private def filterForValidFormIds(
    tree: Tree[BundledFormTreeNode],
    validFormIds: Set[FormIdData]
  ): F[Tree[BundledFormTreeNode]] =
    tree
      .filter(v => validFormIds(v.formIdData))
      .map(_.pure[F])
      .getOrElse(M.raiseError("The status of the root form must be NeedsReview or beyond"))

  private def workOutValidFormIds(tree: Tree[BundledFormTreeNode])(implicit hc: HeaderCarrier): F[Set[FormIdData]] =
    for {
      formStatuses <- tree.toList.traverse(node =>
                        formAlgebra.get(node.formIdData).map { form =>
                          (node.formIdData, isInAnAppropriateState(form.status))
                        }
                      )
    } yield formStatuses.collect { case (id, true) => id }.toSet

  private def isInAnAppropriateState(status: FormStatus): Boolean =
    status match {
      case NeedsReview | Accepting | Returning | Accepted | Submitting | Submitted | Discarded | ManuallySubmitted =>
        true
      case _ => false
    }

  def submitFormBundleAfterReview(rootFormIdData: FormIdData, submissionData: NonEmptyList[BundledFormSubmissionData])(
    implicit hc: HeaderCarrier
  ): F[Unit] =
    for {
      _          <- logger.info(show"submitFormBundleAfterReview(rootFormIdData: $rootFormIdData)").pure[F]
      _          <- formAlgebra.updateFormStatus(rootFormIdData.toFormId, Submitting)
      _          <- logger.info(show"Updated root form status to Submitting").pure[F]
      modelTree  <- createModelTree(rootFormIdData, submissionData)
      _          <- logger.info(show"Built model tree").pure[F]
      submission <- submissionRepoAlgebra.get(rootFormIdData.toFormId.value)
      _          <- logger.info(show"Got submission for rootForm").pure[F]
      submissionInfo = DestinationSubmissionInfo("", submission)
      _ <- destinationsSubmitterAlgebra.send(
             submissionInfo,
             modelTree,
             None,
             LangADT.En,
             FrontEndSubmissionDesIncludeIfEval(submissionData.toList.flatMap(_.desIncludeIfEval.exprEval))
           )
      _ <- logger.info(show"Ran submitter").pure[F]
      _ <- transitionAllChildNodesToSubmitted(modelTree)
      _ <- logger.info(show"Transitioned all child nodes to submitter").pure[F]
    } yield ()

  private def transitionAllChildNodesToSubmitted(
    modelTree: HandlebarsModelTree
  )(implicit hc: HeaderCarrier): F[Unit] = {
    def doTransitions =
      modelTree.toList.tail.traverse { node =>
        forceUpdateFormStatus(node.formId, Submitted)
      }.void

    formAlgebra
      .get(modelTree.value.formId)
      .flatMap { form =>
        if (form.status === Submitted) doTransitions
        else ().pure[F]
      }
  }

  private def createModelTree(rootFormId: FormIdData, submissionData: NonEmptyList[BundledFormSubmissionData])(implicit
    hc: HeaderCarrier
  ): F[HandlebarsModelTree] =
    for {
      formTree      <- formTree(rootFormId)
      _             <- logger.info(show"formTree: $formTree").pure[F]
      formsById     <- buildMap(formTree)(_.formIdData)(findForm)
      templatesById <- buildMap(formTree)(_.formIdData.formTemplateId)(findFormTemplate)
      submissionDataByFormId = submissionData.map(d => (d.formIdData, d)).toList.toMap
      pdfHtmlByFormId <- buildMap(formTree)(_.formIdData)(findSummaryPdf)
      processorModelByFormId <-
        buildProcessorModelByFormId(formTree, formsById, pdfHtmlByFormId, submissionDataByFormId)
    } yield formTree.map { formTreeNode =>
      HandlebarsModelTreeNode(
        formTreeNode.formIdData.toFormId,
        extractSubmissionRef(formTreeNode.formIdData),
        templatesById(formTreeNode.formIdData.formTemplateId),
        processorModelByFormId(formTreeNode.formIdData),
        pdfHtmlByFormId(formTreeNode.formIdData),
        None,
        submissionDataByFormId(formTreeNode.formIdData).structuredFormData
      )
    }

  private def findForm(formId: FormIdData)(implicit hc: HeaderCarrier): F[Form] =
    for {
      _    <- logger.info(show"FormBundleSubmissionService.findForm($formId)").pure[F]
      form <- formAlgebra.get(formId)
      _    <- logger.info(show"FormBundleSubmissionService.findForm($formId) - found").pure[F]
    } yield form

  private def findFormTemplate(id: FormTemplateId): F[FormTemplate] =
    for {
      _        <- logger.info(show"FormBundleSubmissionService.findFormTemplate($id)").pure[F]
      template <- formTemplateAlgebra.get(id)
      _        <- logger.info(show"FormBundleSubmissionService.findFormTemplate($id) - found").pure[F]
    } yield template

  private def findSummaryPdf(id: FormIdData)(implicit hc: HeaderCarrier): F[PdfHtml] =
    for {
      _   <- logger.info(show"FormBundleSubmissionService.findSummaryPdf($id) - formId = ${id.toFormId}").pure[F]
      pdf <- pdfSummaryAlgebra.getLatestPdfHtml(id.toFormId)
      _   <- logger.info(show"FormBundleSubmissionService.findSummaryPdf($id) - found").pure[F]
    } yield pdf

  private def extractSubmissionRef(formIdData: FormIdData): SubmissionRef =
    formIdData match {
      case _: FormIdData.Plain            => SubmissionRef("")
      case wac: FormIdData.WithAccessCode => SubmissionRef(wac.accessCode.value)
    }

  private def buildProcessorModelByFormId(
    formTree: Tree[BundledFormTreeNode],
    formsById: Map[FormIdData, Form],
    pdfHtmlByFormId: Map[FormIdData, PdfHtml],
    submissionDataByFormId: Map[FormIdData, BundledFormSubmissionData]
  )(implicit hc: HeaderCarrier) =
    buildMap(formTree)(_.formIdData) { id =>
      for {
        formTemplate <- formTemplateAlgebra.get(id.formTemplateId)
        baseModel <- destinationProcessorModelAlgebra
                       .create(
                         formsById(id),
                         FrontEndSubmissionVariables(JsObject(Nil)),
                         pdfHtmlByFormId(id),
                         None,
                         submissionDataByFormId(id).structuredFormData,
                         formTemplate.isObjectStore
                       )
        treeModel = DestinationsProcessorModelAlgebra.createBundledFormTree(formTree)
      } yield baseModel + treeModel
    }

  private def buildMap[K, V](
    tree: Tree[BundledFormTreeNode]
  )(key: BundledFormTreeNode => K)(value: K => F[V]): F[Map[K, V]] = {
    val keys = tree
      .fold(Set.empty[K]) { (acc, n) =>
        acc + key(n)
      }
      .toList
    keys.traverse(value).map(values => keys.zip(values).toMap)
  }
}
