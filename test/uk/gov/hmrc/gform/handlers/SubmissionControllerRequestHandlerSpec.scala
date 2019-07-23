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

package uk.gov.hmrc.gform.handlers

import cats.data.EitherT
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.SubmissionData
import uk.gov.hmrc.gform.sharedmodel.form.FormId

import scala.concurrent.Future

class SubmissionControllerRequestHandlerSpec extends Spec {

  it should "handle submission request and submission service response" in new SubmissionControllerRequestHandler {
    val submission: (FormId, String, Option[AffinityGroup], SubmissionData) => FOpt[Unit] =
      (_, _, _, _) => EitherT[Future, UnexpectedState, Unit](Future.successful(Right(())))
    val submissionData = mock[SubmissionData]

    whenReady(handleSubmissionRequest(submission)(FormId("123"), "", None, submissionData).value) { result =>
      result shouldBe Right(())
    }
  }

  it should "handle submission request and submission service error response" in new SubmissionControllerRequestHandler {
    val submission: (FormId, String, Option[AffinityGroup], SubmissionData) => FOpt[Unit] =
      (_, _, _, _) => EitherT[Future, UnexpectedState, Unit](Future.successful(Left(UnexpectedState("wrong"))))
    val submissionData = mock[SubmissionData]

    whenReady(handleSubmissionRequest(submission)(FormId("123"), "", None, submissionData).value.failed) { result =>
      result shouldBe an[Exception]
      result.getMessage shouldBe "wrong"
    }
  }

}
