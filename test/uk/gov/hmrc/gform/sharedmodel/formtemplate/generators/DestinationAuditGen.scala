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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.submission.{ DestinationAudit, SubmissionRef, SummaryHtmlId }

trait DestinationAuditGen {
  def destinationAuditGen: Gen[DestinationAudit] =
    for {
      formId                    <- FormGen.formIdGen
      formTemplateId            <- FormTemplateGen.formTemplateIdGen
      destinationId             <- DestinationGen.destinationIdGen
      destinationType           <- PrimitiveGen.nonEmptyAlphaNumStrGen
      destinationResponseStatus <- Gen.option(Gen.chooseNum(100, 599))
      formStatus                <- FormGen.formStatusGen
      userId                    <- UserIdGen.userIdGen
      caseworkerUserName        <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      parentFormSubmissionRef   <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      submissionRef             <- SubmissionRefGen.submissionRefGen
      summaryHtmlId             <- Gen.uuid.map(SummaryHtmlId(_))
    } yield
      DestinationAudit(
        formId,
        formTemplateId,
        destinationId,
        destinationType,
        destinationResponseStatus,
        formStatus,
        userId,
        caseworkerUserName,
        parentFormSubmissionRef,
        Map.empty,
        submissionRef,
        summaryHtmlId
      )
}

object DestinationAuditGen extends DestinationAuditGen
