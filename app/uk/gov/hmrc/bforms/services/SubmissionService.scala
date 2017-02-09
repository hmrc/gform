/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.services

import cats.instances.future._
import play.api.libs.json.{ JsObject, JsValue, Json }
import uk.gov.hmrc.bforms.core._
import uk.gov.hmrc.bforms.exceptions.InvalidState
import uk.gov.hmrc.bforms.model.{ DbOperationResult, DmsMetaData, Form, FormId, FormTemplate, FormTypeId, SaveAndRetrieve, Submission, SubmissionRef }
import uk.gov.hmrc.bforms.repositories.FormTemplateRepository
import uk.gov.hmrc.bforms.typeclasses.{ Find, FindOne, Insert, Now, ServiceUrl, Update }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import java.time.LocalDateTime
import uk.gov.hmrc.play.http.HeaderCarrier

object SubmissionService {

  def submission(
    formTypeId: FormTypeId,
    formId: FormId
  )(
    implicit
    FindOneForm: FindOne[Form],
    now: Now[LocalDateTime]
  ): ServiceResponse[DbOperationResult] = {
    for {
      form <- FormService.getByTypeAndId(formTypeId, formId)
    } yield {
      Submission(
        submittedDate = now(),
        submissionRef = SubmissionRef.random,
        dmsMetaData = DmsMetaData(
          formId = form._id,
          formNino = None,
          authNino = None,
          classificationType = "???",
          businessArea = "???"
        ),
        submissionMark = None,
        casKey = None
      )
    }

    ???
  }
}
