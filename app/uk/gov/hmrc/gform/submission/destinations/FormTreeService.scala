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

import cats.MonadError
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._
import cats.syntax.traverse._
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.form.BundledFormTreeNode
import uk.gov.hmrc.gform.formmetadata.{ FormMetadata, FormMetadataAlgebra }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.FormIdData
import uk.gov.hmrc.gform.submission.Tree

class FormTreeService[M[_]](formMetadataAlgebra: FormMetadataAlgebra[M])(implicit M: MonadError[M, String])
    extends FormTreeAlgebra[M] {
  private val logger = LoggerFactory.getLogger(getClass)

  def getFormTree(rootFormId: FormIdData): M[Tree[BundledFormTreeNode]] =
    for {
      _    <- logger.info(s"getFormTree(${rootFormId.toFormId.value})").pure[M]
      tree <- formMetadataAlgebra.get(rootFormId).flatMap(buildHierarchyForForm(_, Set.empty))
    } yield tree

  private def buildDescendantTrees(
    metadataForChildren: List[FormMetadata],
    processedSubmissionReferences: Set[SubmissionRef]
  ): M[List[Tree[BundledFormTreeNode]]] =
    metadataForChildren
      .traverse(buildHierarchyForForm(_, processedSubmissionReferences))

  private def buildDescendantTreesForSubmissionRef(
    submissionRef: SubmissionRef,
    processedSubmissionReferences: Set[SubmissionRef]
  ): M[List[Tree[BundledFormTreeNode]]] =
    for {
      _             <- logger.info(s"buildDescendantTreesForSubmissionRef(${submissionRef.value})").pure[M]
      _             <- verifyNoLoops(submissionRef, processedSubmissionReferences)
      childMetadata <- formMetadataAlgebra.findByParentFormSubmissionRef(submissionRef)
      _ =
        logger.info(
          s"buildDescendantTreesForSubmissionRef(${submissionRef.value}) - child submission references: [${childMetadata
            .map(_.submissionRef.fold("")(_.value))
            .mkString(", ")}]"
        )
      descendantTrees <- buildDescendantTrees(childMetadata, processedSubmissionReferences + submissionRef)
    } yield descendantTrees

  private def buildDescendantTreesForSubmissionRef(
    maybeSubmissionRef: Option[SubmissionRef],
    processedSubmissionReferences: Set[SubmissionRef]
  ): M[List[Tree[BundledFormTreeNode]]] =
    maybeSubmissionRef
      .map(buildDescendantTreesForSubmissionRef(_, processedSubmissionReferences))
      .getOrElse(List.empty.pure[M])

  private def buildHierarchyForForm(
    metadata: FormMetadata,
    processedSubmissionReferences: Set[SubmissionRef]
  ): M[Tree[BundledFormTreeNode]] =
    for {
      _     <- logger.info(show"buildHierarchyForForm(${metadata._id})").pure[M]
      trees <- buildDescendantTreesForSubmissionRef(metadata.submissionRef, processedSubmissionReferences)
    } yield Tree(BundledFormTreeNode(metadata.formIdData), trees)

  private def verifyNoLoops(submissionRef: SubmissionRef, processedSubmissionReferences: Set[SubmissionRef]): M[Unit] =
    if (processedSubmissionReferences.contains(submissionRef))
      M.raiseError(FormTreeService.cycleErrorMessage(submissionRef))
    else ().pure[M]
}

object FormTreeService {
  def cycleErrorMessage(submissionRef: SubmissionRef): String =
    s"A cycle of submission references has been detected involving ${submissionRef.value}"
}
