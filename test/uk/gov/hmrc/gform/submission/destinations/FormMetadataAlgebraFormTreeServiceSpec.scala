/*
 * Copyright 2022 HM Revenue & Customs
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

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.{ Possible, Spec, possibleMonadError }
import uk.gov.hmrc.gform.form.BundledFormTreeNode
import uk.gov.hmrc.gform.formmetadata.{ FormMetadata, FormMetadataAlgebra, FormMetadataGen }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.FormIdData
import uk.gov.hmrc.gform.submission.Tree

class FormMetadataAlgebraFormTreeServiceSpec extends Spec with FormMetadataGen with ScalaCheckDrivenPropertyChecks {
  "getFormTree" should "return a single level if there are no children" in {
    forAll(formMetadataGenWithSubmissionRef) { generatedAudit =>
      val rootAudit = generatedAudit.copy(parentFormSubmissionRefs = Nil)

      createService()
        .expectMetaDataAlgebraGet(rootAudit.formIdData, rootAudit)
        .expectMetadataAlgebraFindByParentFormSubmissionRef(rootAudit.submissionRef.get, Nil)
        .service
        .getFormTree(rootAudit.formIdData)
        .right
        .value shouldBe
        Tree(BundledFormTreeNode(rootAudit.formIdData))
    }
  }

  it must "return a root-child tree" in {
    forAll(formMetadataGenWithSubmissionRef, formMetadataGenWithSubmissionRef) { (generatedRoot, generatedChild) =>
      val root = generatedRoot.copy(parentFormSubmissionRefs = Nil)
      val child = generatedChild.copy(parentFormSubmissionRefs = List(root.submissionRef.value))

      whenever(distinct(root, child)(_.submissionRef)) {
        createService()
          .expectMetaDataAlgebraGet(root.formIdData, root)
          .expectMetadataAlgebraFindByParentFormSubmissionRef(root.submissionRef.get, List(child))
          .expectMetadataAlgebraFindByParentFormSubmissionRef(child.submissionRef.get, Nil)
          .service
          .getFormTree(root.formIdData)
          .right
          .value shouldBe
          formTree(root, formTree(child))
      }
    }
  }

  it must "return a root-child-grandchild tree" in {
    forAll(formMetadataGenWithSubmissionRef, formMetadataGenWithSubmissionRef, formMetadataGenWithSubmissionRef) {
      (generatedRoot, generatedChild, generatedGrandchild) =>
        val root = generatedRoot.copy(parentFormSubmissionRefs = Nil)
        val child = generatedChild.copy(parentFormSubmissionRefs = List(root.submissionRef.value))
        val grandchild = generatedGrandchild.copy(parentFormSubmissionRefs = List(child.submissionRef.value))

        whenever(
          distinct(root, child, grandchild)(_.submissionRef) &&
            distinct(root, child, grandchild)(_.parentFormSubmissionRefs)
        ) {
          createService()
            .expectMetaDataAlgebraGet(root.formIdData, root)
            .expectMetadataAlgebraFindByParentFormSubmissionRef(root.submissionRef.get, List(child))
            .expectMetadataAlgebraFindByParentFormSubmissionRef(child.submissionRef.get, List(grandchild))
            .expectMetadataAlgebraFindByParentFormSubmissionRef(grandchild.submissionRef.get, Nil)
            .service
            .getFormTree(root.formIdData)
            .right
            .value shouldBe
            formTree(root, formTree(child, formTree(grandchild)))
        }
    }
  }

  it must "group children by their form template ids" in {
    forAll(
      formMetadataGenWithSubmissionRef,
      formMetadataGenWithSubmissionRef,
      formMetadataGenWithSubmissionRef,
      formMetadataGenWithSubmissionRef
    ) { (generatedRoot, generatedChild1, generatedChild2, generatedChild3) =>
      val root = generatedRoot.copy(parentFormSubmissionRefs = Nil)
      val child1 = generatedChild1.copy(parentFormSubmissionRefs = List(root.submissionRef.value))
      val child2 = generatedChild2
        .copy(parentFormSubmissionRefs = List(root.submissionRef.value), formTemplateId = child1.formTemplateId)
      val child3 = generatedChild3.copy(parentFormSubmissionRefs = List(root.submissionRef.value))

      whenever(
        distinct(root, child1, child2, child3)(_.submissionRef) &&
          distinct(child1, child3)(_.formTemplateId)
      ) {
        createService()
          .expectMetaDataAlgebraGet(root.formIdData, root)
          .expectMetadataAlgebraFindByParentFormSubmissionRef(root.submissionRef.get, List(child1, child2, child3))
          .expectMetadataAlgebraFindByParentFormSubmissionRef(child1.submissionRef.get, Nil)
          .expectMetadataAlgebraFindByParentFormSubmissionRef(child2.submissionRef.get, Nil)
          .expectMetadataAlgebraFindByParentFormSubmissionRef(child3.submissionRef.get, Nil)
          .service
          .getFormTree(root.formIdData)
          .right
          .value shouldBe
          formTree(root, formTree(child1), formTree(child2), formTree(child3))
      }
    }
  }

  it must "detect cycles" in {
    forAll(formMetadataGenWithSubmissionRef, formMetadataGenWithSubmissionRef, formMetadataGenWithSubmissionRef) {
      (generatedRoot, generatedChild, generatedGrandchild) =>
        val root = generatedRoot.copy(parentFormSubmissionRefs = Nil)
        val child = generatedChild.copy(
          parentFormSubmissionRefs = List(root.submissionRef.value, generatedGrandchild.submissionRef.value)
        )
        val grandchild = generatedGrandchild.copy(parentFormSubmissionRefs = List(child.submissionRef.value))

        whenever(
          distinct(root, child, grandchild)(_.submissionRef) &&
            distinct(root, child, grandchild)(_.parentFormSubmissionRefs)
        ) {
          createService()
            .expectMetaDataAlgebraGet(root.formIdData, root)
            .expectMetadataAlgebraFindByParentFormSubmissionRef(root.submissionRef.get, List(child))
            .expectMetadataAlgebraFindByParentFormSubmissionRef(child.submissionRef.get, List(grandchild))
            .expectMetadataAlgebraFindByParentFormSubmissionRef(grandchild.submissionRef.get, List(child))
            .service
            .getFormTree(root.formIdData)
            .left
            .value shouldBe FormTreeService.cycleErrorMessage(child.submissionRef.get)
        }
    }
  }

  private def distinct[T](audits: FormMetadata*)(f: FormMetadata => T) =
    audits.map(f).toSet.size == audits.size

  private def formTree(formIdData: FormIdData, children: Tree[BundledFormTreeNode]*): Tree[BundledFormTreeNode] =
    Tree(BundledFormTreeNode(formIdData), children: _*)

  private def formTree(formMetadata: FormMetadata, children: Tree[BundledFormTreeNode]*): Tree[BundledFormTreeNode] =
    formTree(formMetadata.formIdData, children: _*)

  case class Fixture(service: FormTreeService[Possible], metadataAlgebra: FormMetadataAlgebra[Possible]) {
    def expectMetaDataAlgebraGet(formIdData: FormIdData, metadata: FormMetadata): Fixture = {
      (metadataAlgebra
        .get(_: FormIdData))
        .expects(formIdData)
        .returning(Right(metadata))
      this
    }

    def expectMetadataAlgebraFindByParentFormSubmissionRef(
      submissionRef: SubmissionRef,
      childMetadata: List[FormMetadata]
    ): Fixture = {
      (metadataAlgebra
        .findByParentFormSubmissionRef(_: SubmissionRef))
        .expects(submissionRef)
        .returning(Right(childMetadata))
      this
    }
  }

  def createService(): Fixture = {
    val metadataAlgebra = mock[FormMetadataAlgebra[Possible]]
    Fixture(new FormTreeService[Possible](metadataAlgebra), metadataAlgebra)
  }
}
