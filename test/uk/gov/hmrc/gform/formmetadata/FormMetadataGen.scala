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

import java.time.Instant

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ FormGen, FormTemplateGen, PrimitiveGen, SubmissionRefGen, UserIdGen }

trait FormMetadataGen {
  def formMetadataGen: Gen[FormMetadata] =
    for {
      formId                   <- FormGen.formIdGen
      userId                   <- UserIdGen.userIdGen
      formTemplateId           <- FormTemplateGen.formTemplateIdGen
      submissionRef            <- Gen.option(SubmissionRefGen.submissionRefGen)
      parentFormSubmissionRefs <- PrimitiveGen.zeroOrMoreGen(SubmissionRefGen.submissionRefGen)
    } yield
      FormMetadata(formId, userId, formTemplateId, submissionRef, parentFormSubmissionRefs, Instant.now, Instant.now)

  def formMetadataGenWithSubmissionRef: Gen[FormMetadata] =
    for {
      formId                   <- FormGen.formIdGen
      userId                   <- UserIdGen.userIdGen
      formTemplateId           <- FormTemplateGen.formTemplateIdGen
      submissionRef            <- SubmissionRefGen.submissionRefGen
      parentFormSubmissionRefs <- PrimitiveGen.zeroOrMoreGen(SubmissionRefGen.submissionRefGen)
    } yield
      FormMetadata(
        formId,
        userId,
        formTemplateId,
        Some(submissionRef),
        parentFormSubmissionRefs,
        Instant.now,
        Instant.now)
}

object FormMetadataGen extends FormMetadataGen
