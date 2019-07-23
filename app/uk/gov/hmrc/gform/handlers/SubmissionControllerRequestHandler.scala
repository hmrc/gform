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

import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA, _ }
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil.toAffinityGroupO
import uk.gov.hmrc.gform.sharedmodel.SubmissionData
import uk.gov.hmrc.gform.sharedmodel.form.FormId

import scala.concurrent.ExecutionContext

trait SubmissionControllerRequestHandler {

  def handleSubmissionRequest(submissionWithPdf: (FormId, String, Option[AffinityGroup], SubmissionData) => FOpt[Unit])(
    formId: FormId,
    customerId: String,
    affinityGroupHeader: Option[String],
    submissionData: SubmissionData)(implicit ex: ExecutionContext): FOpt[Unit] =
    fromFutureA(
      submissionWithPdf(
        formId,
        customerId,
        toAffinityGroupO(affinityGroupHeader),
        submissionData
      ).toFuture)
}
