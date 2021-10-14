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

package uk.gov.hmrc.gform.formmetadata

import cats.instances.future._
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.option._
import cats.syntax.show._
import org.mongodb.scala.model.Filters._
import java.time.Instant

import org.slf4j.LoggerFactory
import play.api.libs.json.JsString

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SubmissionRef, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.FormIdData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

trait FormMetadataAlgebra[F[_]] {
  def get(formIdData: FormIdData): F[FormMetadata]
  def getAll(userId: UserId, formTemplateId: FormTemplateId): F[List[FormMetadata]]
  def upsert(formIdData: FormIdData): F[Unit]
  def touch(formIdData: FormIdData, parentFormSubmissionRefs: List[SubmissionRef]): F[Unit]
  def findByParentFormSubmissionRef(parentFormSubmissionRef: SubmissionRef): F[List[FormMetadata]]
  def delete(formIdData: FormIdData): F[Unit]
}

class FormMetadataService(formMetadataRepo: Repo[FormMetadata])(implicit ec: ExecutionContext)
    extends FormMetadataAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  private def find(formIdData: FormIdData): Future[Option[FormMetadata]] =
    formMetadataRepo.find(formIdData.toFormId.value)

  def get(formIdData: FormIdData): Future[FormMetadata] = formMetadataRepo.get(formIdData.toFormId.value)

  def getAll(userId: UserId, formTemplateId: FormTemplateId): Future[List[FormMetadata]] = formMetadataRepo.search(
    and(
      equal("userId", userId.value),
      equal("formTemplateId", formTemplateId.value)
    )
  )

  def touch(formIdData: FormIdData, parentFormSubmissionRefs: List[SubmissionRef]): Future[Unit] = {
    val now = Instant.now()
    for {
      maybeFormMetadata <- find(formIdData)
      newMetadata = maybeFormMetadata.fold(newFormMetadata(formIdData)) {
                      _.copy(
                        updatedAt = now,
                        parentFormSubmissionRefs = parentFormSubmissionRefs
                      )
                    }
      _ <- toFuture(formMetadataRepo.upsert(newMetadata))
      _ <- Future.successful {
             logger.info(
               show"FormMetadataService.touch($formIdData, $parentFormSubmissionRefs) - updating with $newMetadata)"
             )
           }
    } yield ()
  }

  def upsert(formIdData: FormIdData): Future[Unit] = {
    val metadata = newFormMetadata(formIdData)
    toFuture(formMetadataRepo.upsert(metadata)) >>
      Future.successful(logger.info(show"FormMetadataService.upsert($formIdData) - upserting $metadata)"))
  }

  def findByParentFormSubmissionRef(parentFormSubmissionRef: SubmissionRef): Future[List[FormMetadata]] = {
    val parentSubmissionRefJson = JsString(parentFormSubmissionRef.value)
    val query = and(
      notEqual("submissionRef", parentSubmissionRefJson),
      in("parentFormSubmissionRefs", parentSubmissionRefJson)
    )
    formMetadataRepo.search(query)
  }

  private def newFormMetadata(formIdData: FormIdData): FormMetadata = {
    val now = Instant.now()
    formIdData match {
      case FormIdData.Plain(userId, formTemplateId) =>
        FormMetadata(formIdData.toFormId, userId, formTemplateId, None, List.empty, now, now)
      case FormIdData.WithAccessCode(userId, formTemplateId, AccessCode(accessCode)) =>
        FormMetadata(
          formIdData.toFormId,
          userId,
          formTemplateId,
          SubmissionRef(accessCode).some,
          List.empty,
          now,
          now
        )
    }
  }

  private def toFuture[A](fOpt: FOpt[A]): Future[A] =
    fOpt.value
      .flatMap {
        case Left(UnexpectedState(error)) => Future.failed(new Exception(error))
        case Right(unit)                  => Future.successful(unit)
      }

  override def delete(formIdData: FormIdData): Future[Unit] = {
    val metadata = newFormMetadata(formIdData)
    toFuture(
      formMetadataRepo
        .delete(metadata.formIdData.toFormId.value)
        .as(logger.info(show"FormMetadataService.delete($formIdData) - deleting $metadata)"))
    )
  }
}
