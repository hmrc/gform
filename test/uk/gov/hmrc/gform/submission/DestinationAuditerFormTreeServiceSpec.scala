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

import cats.Id
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.form.BundledFormTreeNode
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.DestinationAuditGen._
import uk.gov.hmrc.http.HeaderCarrier

class DestinationAuditerFormTreeServiceSpec extends Spec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  "getFormTree" should "return a single level if there are no children" in {
    forAll(destinationAuditGen) { generatedAudit =>
      val rootAudit = generatedAudit.copy(parentFormSubmissionRef = None)

      createService()
        .expectAuditAlgebraGetLatestForForm(rootAudit.formId, rootAudit)
        .expectFindLatestChildAudits(rootAudit.submissionRef, Nil)
        .service
        .getFormTree(rootAudit.formId) shouldBe
        Tree(BundledFormTreeNode(rootAudit.formId, rootAudit.submissionRef, rootAudit.formTemplateId))
    }
  }

  it must "return a root-child tree" in {
    forAll(destinationAuditGen, destinationAuditGen) { (generatedRoot, generatedChild) =>
      val root = generatedRoot.copy(parentFormSubmissionRef = None)
      val child = generatedChild.copy(parentFormSubmissionRef = Some(root.submissionRef.value))

      whenever(distinct(root, child)(_.submissionRef)) {
        createService()
          .expectAuditAlgebraGetLatestForForm(root.formId, root)
          .expectFindLatestChildAudits(root.submissionRef, List(child))
          .expectFindLatestChildAudits(child.submissionRef, Nil)
          .service
          .getFormTree(root.formId) shouldBe
          formTree(root, formTree(child))
      }
    }
  }

  it must "return a root-child-grandchild tree" in {
    forAll(destinationAuditGen, destinationAuditGen, destinationAuditGen) {
      (generatedRoot, generatedChild, generatedGrandchild) =>
        val root = generatedRoot.copy(parentFormSubmissionRef = None)
        val child = generatedChild.copy(parentFormSubmissionRef = Some(root.submissionRef.value))
        val grandchild = generatedGrandchild.copy(parentFormSubmissionRef = Some(child.submissionRef.value))

        whenever(
          distinct(root, child, grandchild)(_.submissionRef) &&
            distinct(root, child, grandchild)(_.parentFormSubmissionRef)) {
          createService()
            .expectAuditAlgebraGetLatestForForm(root.formId, root)
            .expectFindLatestChildAudits(root.submissionRef, List(child))
            .expectFindLatestChildAudits(child.submissionRef, List(grandchild))
            .expectFindLatestChildAudits(grandchild.submissionRef, Nil)
            .service
            .getFormTree(root.formId) shouldBe
            formTree(root, formTree(child, formTree(grandchild)))
        }
    }
  }

  it must "group children by their form template ids" in {
    forAll(destinationAuditGen, destinationAuditGen, destinationAuditGen, destinationAuditGen) {
      (generatedRoot, generatedChild1, generatedChild2, generatedChild3) =>
        val root = generatedRoot.copy(parentFormSubmissionRef = None)
        val child1 = generatedChild1.copy(parentFormSubmissionRef = Some(root.submissionRef.value))
        val child2 = generatedChild2
          .copy(parentFormSubmissionRef = Some(root.submissionRef.value), formTemplateId = child1.formTemplateId)
        val child3 = generatedChild3.copy(parentFormSubmissionRef = Some(root.submissionRef.value))

        whenever(
          distinct(root, child1, child2, child3)(_.submissionRef) &&
            distinct(child1, child3)(_.formTemplateId)) {
          createService()
            .expectAuditAlgebraGetLatestForForm(root.formId, root)
            .expectFindLatestChildAudits(root.submissionRef, List(child1, child2, child3))
            .expectFindLatestChildAudits(child1.submissionRef, Nil)
            .expectFindLatestChildAudits(child2.submissionRef, Nil)
            .expectFindLatestChildAudits(child3.submissionRef, Nil)
            .service
            .getFormTree(root.formId) shouldBe
            formTree(root, formTree(child1), formTree(child2), formTree(child3))
        }
    }
  }

  private def distinct[T](audits: DestinationAudit*)(f: DestinationAudit => T) =
    audits.map(f).toSet.size == audits.size

  private def formTree(audit: DestinationAudit, children: Tree[BundledFormTreeNode]*): Tree[BundledFormTreeNode] =
    Tree(BundledFormTreeNode(audit.formId, audit.submissionRef, audit.formTemplateId), children: _*)

  case class Fixture(service: DestinationAuditerFormTreeService[Id], auditAlgebra: DestinationAuditAlgebra[Id]) {
    def expectAuditAlgebraGetLatestForForm(formId: FormId, audit: DestinationAudit): Fixture = {
      (auditAlgebra
        .getLatestForForm(_: FormId)(_: HeaderCarrier))
        .expects(formId, hc)
        .returning(audit)
      this
    }

    def expectFindLatestChildAudits(submissionRef: SubmissionRef, childAudits: List[DestinationAudit]): Fixture = {
      (auditAlgebra
        .findLatestChildAudits(_: SubmissionRef))
        .expects(submissionRef)
        .returning(childAudits)
      this
    }
  }

  def createService(): Fixture = {
    val auditAlgebra = mock[DestinationAuditAlgebra[Id]]
    Fixture(new DestinationAuditerFormTreeService(auditAlgebra), auditAlgebra)
  }
}
