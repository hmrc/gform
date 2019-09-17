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

package uk.gov.hmrc.gform.formmetadata

import cats.syntax.option._
import java.time.Instant
import play.api.libs.json.Json
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SubmissionRef, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

trait FormMetadataAlgebra[F[_]] {
  //def get(formIdData: FormIdData): F[FormMetadata]
  def getAll(userId: UserId, formTemplateId: FormTemplateId): F[List[FormMetadata]]
  def upsert(formIdData: FormIdData): F[Unit]
  def touch(formIdData: FormIdData, parentFormSubmissionRefs: List[SubmissionRef]): F[Unit]
}

class FormMetadataService(formMetadataRepo: FormMetadataRepo)(implicit ec: ExecutionContext)
    extends FormMetadataAlgebra[Future] {

  private def get(formIdData: FormIdData): Future[FormMetadata] = formMetadataRepo.get(formIdData.toFormId.value)

  def getAll(userId: UserId, formTemplateId: FormTemplateId): Future[List[FormMetadata]] = formMetadataRepo.search(
    Json.obj(
      "userId"         -> userId.value,
      "formTemplateId" -> formTemplateId.value
    )
  )

  def touch(formIdData: FormIdData, parentFormSubmissionRefs: List[SubmissionRef]): Future[Unit] = {
    val now = Instant.now()
    for {
      formMetadata <- get(formIdData)
      _ <- toFuture(
            formMetadataRepo.upsert(
              formMetadata.copy(
                updatedAt = now,
                parentFormSubmissionRefs = parentFormSubmissionRefs
              )
            )
          )
    } yield ()
  }

  def upsert(formIdData: FormIdData): Future[Unit] =
    toFuture(formMetadataRepo.upsert {
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
    })

  private def toFuture[A](fOpt: FOpt[A]): Future[A] =
    fOpt.value
      .flatMap {
        case Left(UnexpectedState(error)) => Future.failed(new Exception(error))
        case Right(unit)                  => Future.successful(unit)
      }
}
