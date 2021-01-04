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

import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.{ SubmissionRef, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.FormIdData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

class FormMetadataModule(mongoModule: MongoModule)(implicit ex: ExecutionContext) {
  private val formMetadataRepo: Repo[FormMetadata] =
    new Repo[FormMetadata]("formMetadata", mongoModule.mongo, _._id.value)
  val formMetadataService: FormMetadataAlgebra[Future] = new FormMetadataService(formMetadataRepo)

  val foptFormMetadataService: FormMetadataAlgebra[FOpt] = new FormMetadataAlgebra[FOpt] {
    override def get(formIdData: FormIdData): FOpt[FormMetadata] =
      fromFutureA(formMetadataService.get(formIdData))

    override def getAll(userId: UserId, formTemplateId: FormTemplateId): FOpt[List[FormMetadata]] =
      fromFutureA(formMetadataService.getAll(userId, formTemplateId))

    override def upsert(formIdData: FormIdData): FOpt[Unit] =
      fromFutureA(formMetadataService.upsert(formIdData))

    override def touch(formIdData: FormIdData, parentFormSubmissionRefs: List[SubmissionRef]): FOpt[Unit] =
      fromFutureA(formMetadataService.touch(formIdData, parentFormSubmissionRefs))

    override def findByParentFormSubmissionRef(parentFormSubmissionRef: SubmissionRef): FOpt[List[FormMetadata]] =
      fromFutureA(formMetadataService.findByParentFormSubmissionRef(parentFormSubmissionRef))
  }
}
