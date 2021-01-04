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

package uk.gov.hmrc.gform.submission.destinations

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ FormGen, FormTemplateGen, PrimitiveGen }

trait DestinationSubmissionInfoGen {
  def destinationSubmissionInfoGen: Gen[DestinationSubmissionInfo] =
    for {
      formTemplate  <- FormTemplateGen.formTemplateGen
      formId        <- FormGen.formIdGen
      customerId    <- PrimitiveGen.nonEmptyAlphaNumStrGen
      submissionRef <- PrimitiveGen.nonEmptyAlphaNumStrGen.map(SubmissionRef(_))
      submission <- SubmissionGen.submissionGen.map(s => {
                     s.copy(
                       _id = formId,
                       submissionRef = submissionRef,
                       dmsMetaData = s.dmsMetaData.copy(customerId = customerId, formTemplateId = formTemplate._id))
                   })
    } yield
      DestinationSubmissionInfo(
        customerId,
        submission
      )
}

object DestinationSubmissionInfoGen extends DestinationSubmissionInfoGen
