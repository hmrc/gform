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

package uk.gov.hmrc.gform.submission.destinations

import cats.MonadError
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import play.api.Logger
import uk.gov.hmrc.gform.form.BundledFormTreeNode
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.submission.Tree
import uk.gov.hmrc.http.HeaderCarrier

class FormTreeService[M[_]](auditAlgebra: DestinationAuditAlgebra[M])(implicit M: MonadError[M, String])
    extends FormTreeAlgebra[M] {
  def getFormTree(rootFormId: FormId)(implicit hc: HeaderCarrier): M[Tree[BundledFormTreeNode]] =
    for {
      _    <- Logger.info(s"getFormTree(${rootFormId.value})").pure[M]
      tree <- auditAlgebra.getLatestForForm(rootFormId).flatMap(buildHierarchyForAudit(_, Set.empty))
    } yield tree

  private def buildDescendantTreesForChildAudits(
    childAudits: List[DestinationAudit],
    processedSubmissionReferences: Set[SubmissionRef]) =
    childAudits
      .traverse(buildHierarchyForAudit(_, processedSubmissionReferences))

  private def buildDescendantTreesForSubmissionRef(
    submissionRef: SubmissionRef,
    processedSubmissionReferences: Set[SubmissionRef]) =
    for {
      _           <- Logger.info(s"buildDescendantTreesForSubmissionRef(${submissionRef.value})").pure[M]
      _           <- verifyNoLoops(submissionRef, processedSubmissionReferences)
      childAudits <- auditAlgebra.findLatestChildAudits(submissionRef)
      _ = Logger.info(
        s"buildDescendantTreesForSubmissionRef(${submissionRef.value}) - child submission references: [${childAudits.map(_.submissionRef.value).mkString(", ")}]")
      descendantTrees <- buildDescendantTreesForChildAudits(childAudits, processedSubmissionReferences + submissionRef)
    } yield descendantTrees

  private def buildHierarchyForAudit(
    audit: DestinationAudit,
    processedSubmissionReferences: Set[SubmissionRef]): M[Tree[BundledFormTreeNode]] =
    for {
      _     <- Logger.info(s"buildHierarchyForAudit(${audit.submissionRef.value})").pure[M]
      trees <- buildDescendantTreesForSubmissionRef(audit.submissionRef, processedSubmissionReferences)
    } yield Tree(BundledFormTreeNode(audit.formId, audit.submissionRef, audit.formTemplateId), trees)

  private def verifyNoLoops(submissionRef: SubmissionRef, processedSubmissionReferences: Set[SubmissionRef]): M[Unit] =
    if (processedSubmissionReferences.contains(submissionRef))
      M.raiseError(FormTreeService.cycleErrorMessage(submissionRef))
    else ().pure[M]
}

object FormTreeService {
  def cycleErrorMessage(submissionRef: SubmissionRef): String =
    s"A cycle of submission references has been detected involving ${submissionRef.value}"
}
