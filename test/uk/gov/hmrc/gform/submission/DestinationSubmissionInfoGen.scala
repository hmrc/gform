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

package uk.gov.hmrc.gform.submission

import java.time.LocalDateTime

import org.scalacheck.Gen
import play.api.libs.json.JsObject
import uk.gov.hmrc.gform.sharedmodel.formtemplate.EmailParametersRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ FormGen, PrimitiveGen }
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, FieldName, StructuredFormValue }
import uk.gov.hmrc.gform.sharedmodel.{ FrontEndSubmissionVariables, SubmissionData }

trait DestinationSubmissionInfoGen {
  def destinationSubmissionInfoGen: Gen[DestinationSubmissionInfo] =
    for {
      formId        <- FormGen.formIdGen
      customerId    <- PrimitiveGen.nonEmptyAlphaNumStrGen
      submissionRef <- PrimitiveGen.nonEmptyAlphaNumStrGen.map(SubmissionRef(_))
    } yield
      DestinationSubmissionInfo(
        customerId,
        None,
        Submission(formId, LocalDateTime.now, submissionRef, null, 0, null)
      )
}

object DestinationSubmissionInfoGen extends DestinationSubmissionInfoGen
