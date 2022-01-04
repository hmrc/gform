/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ FormGen, PrimitiveGen }
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission, SubmissionId }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

trait SubmissionGen {

  val dmsMetataGen: Gen[DmsMetaData] = for {
    formTemplateId <- PrimitiveGen.nonEmptyAlphaNumStrGen.map(FormTemplateId(_))
    customerId     <- PrimitiveGen.nonEmptyAlphaNumStrGen
  } yield DmsMetaData(formTemplateId, customerId)

  val submissionGen: Gen[Submission] =
    for {
      formId             <- FormGen.formIdGen
      submissionRef      <- PrimitiveGen.nonEmptyAlphaNumStrGen.map(SubmissionRef(_))
      submittedDate      <- PrimitiveGen.localDateTimeGen
      envelopedId        <- Gen.uuid.map(uuid => EnvelopeId(uuid.toString))
      numberOfAttacments <- Gen.choose(1, 10)
      dmsMetadata        <- dmsMetataGen
    } yield Submission(
      SubmissionId(formId, envelopedId),
      submittedDate,
      submissionRef,
      envelopedId,
      numberOfAttacments,
      dmsMetadata
    )
}

object SubmissionGen extends SubmissionGen
