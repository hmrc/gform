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

import cats.Monad
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

class FormTreeService[M[_]: Monad](auditAlgebra: DestinationAuditAlgebra[M]) extends FormTreeAlgebra[M] {
  def getFormTree(rootFormId: FormId)(implicit hc: HeaderCarrier): M[Tree[BundledFormTreeNode]] =
    for {
      _    <- Logger.info(s"getFormTree(${rootFormId.value})").pure[M]
      tree <- auditAlgebra.getLatestForForm(rootFormId).flatMap(buildHierarchyForAudit)
    } yield tree

  private def buildDescendantTreesForChildAudits(childAudits: List[DestinationAudit]) =
    childAudits
      .traverse(buildHierarchyForAudit)

  private def buildDescendantTreesForSubmissionRef(submissionRef: SubmissionRef) =
    for {
      _           <- Logger.info(s"buildDescendantTreesForSubmissionRef(${submissionRef.value})").pure[M]
      childAudits <- auditAlgebra.findLatestChildAudits(submissionRef)
      _ = Logger.info(
        s"buildDescendantTreesForSubmissionRef(${submissionRef.value}) - child submission references: [${childAudits.map(_.submissionRef.value).mkString(", ")}]")
      descendantTrees <- buildDescendantTreesForChildAudits(childAudits)
    } yield descendantTrees

  private def buildHierarchyForAudit(audit: DestinationAudit): M[Tree[BundledFormTreeNode]] =
    for {
      _     <- Logger.info(s"buildHierarchyForAudit(${audit.submissionRef.value})").pure[M]
      trees <- buildDescendantTreesForSubmissionRef(audit.submissionRef)
    } yield Tree(BundledFormTreeNode(audit.formId, audit.submissionRef, audit.formTemplateId), trees)
}
