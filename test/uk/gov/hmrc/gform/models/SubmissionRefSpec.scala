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

package uk.gov.hmrc.gform.models

import uk.gov.hmrc.gform.Spec

class SubmissionRefSpec extends Spec {

  "SubmissionRef.createSubmissionRef" should "generate submissionRef" in {
    val rnd = new scala.util.Random(12)
    val submissionRef = SubmissionRef.createSubmissionRef(rnd)

    submissionRef.value should be("46Q-Z2HW-XIB")

  }
}
