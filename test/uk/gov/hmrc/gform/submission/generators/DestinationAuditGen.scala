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

package uk.gov.hmrc.gform.submission.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.form.FormStatus
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, FormGen, FormTemplateGen, PrimitiveGen, SubmissionRefGen, UserIdGen }
import uk.gov.hmrc.gform.submission.{ DestinationAudit, SummaryHtmlId }

trait DestinationAuditGen {
  def destinationAuditGen(): Gen[DestinationAudit] =
    for {
      formId                    <- FormGen.formIdGen
      formTemplateId            <- FormTemplateGen.formTemplateIdGen
      destinationId             <- DestinationGen.destinationIdGen
      destinationType           <- PrimitiveGen.nonEmptyAlphaNumStrGen
      destinationResponseStatus <- Gen.option(Gen.chooseNum(100, 599))
      workflowState             <- Gen.oneOf(FormStatus.all.toList)
      userId                    <- UserIdGen.userIdGen
      caseworkerUserName        <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      parentFormSubmissionRefs  <- PrimitiveGen.zeroOrMoreGen(PrimitiveGen.nonEmptyAlphaNumStrGen)
      reviewData <- PrimitiveGen
                     .possiblyEmptyMapGen(PrimitiveGen.nonEmptyAlphaNumStrGen, PrimitiveGen.nonEmptyAlphaNumStrGen)
      submissionRef <- SubmissionRefGen.submissionRefGen
      summaryHtmlId <- Gen.uuid.map(SummaryHtmlId(_))
      id            <- Gen.uuid
      timestamp     <- PrimitiveGen.jodaLocalDateTimeGen
    } yield
      DestinationAudit(
        formId,
        formTemplateId,
        destinationId,
        destinationType,
        destinationResponseStatus,
        workflowState,
        userId,
        caseworkerUserName,
        parentFormSubmissionRefs,
        reviewData,
        submissionRef,
        summaryHtmlId,
        id,
        timestamp
      )
}
