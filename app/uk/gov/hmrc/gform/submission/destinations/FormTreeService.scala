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
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import uk.gov.hmrc.gform.form.BundledFormTreeNode
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.submission.Tree
import uk.gov.hmrc.http.HeaderCarrier

class FormTreeService[M[_]: Monad](auditAlgebra: DestinationAuditAlgebra[M]) extends FormTreeAlgebra[M] {
  def getFormTree(rootFormId: FormId)(implicit hc: HeaderCarrier): M[Tree[BundledFormTreeNode]] =
    auditAlgebra.getLatestForForm(rootFormId).flatMap(buildHierarchyForAudit)

  private def buildDescendantTreesForChildAudits(childAudits: List[DestinationAudit]) =
    childAudits
      .traverse(buildHierarchyForAudit)

  private def buildDescendantTreesForSubmissionRef(submissionRef: SubmissionRef) =
    auditAlgebra.findLatestChildAudits(submissionRef).flatMap(buildDescendantTreesForChildAudits)

  private def buildHierarchyForAudit(audit: DestinationAudit): M[Tree[BundledFormTreeNode]] =
    buildDescendantTreesForSubmissionRef(audit.submissionRef)
      .map(Tree(BundledFormTreeNode(audit.formId, audit.submissionRef, audit.formTemplateId), _))
}
