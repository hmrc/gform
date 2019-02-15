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

package uk.gov.hmrc.gform.sharedmodel

import java.time.LocalDateTime

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.fileupload.ReconciliationId
import uk.gov.hmrc.gform.submission.SubmissionRef
import uk.gov.hmrc.gform.typeclasses.Now

class ReconciliationIdSpec extends Spec {

  "ReconciliationId.create" should "generate reconciliationId based on submissionRef and current time" in {

    val rnd = new scala.util.Random(123)

    implicit val now = Now(LocalDateTime.of(2017, 1, 31, 13, 53, 45))

    val submissionRef = SubmissionRef.createSubmissionRef(envelopeId)

    val res = ReconciliationId.create(submissionRef)

    res.value should be("6FJXHVQLU4FD-20170131135345")

  }
}
