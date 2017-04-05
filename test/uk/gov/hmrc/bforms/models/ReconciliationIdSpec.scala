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

package uk.gov.hmrc.bforms.models

import java.time.LocalDateTime
import org.scalatest._
import uk.gov.hmrc.bforms.typeclasses.Now

class ReconciliationIdSpec extends FlatSpec with Matchers {

  "ReconciliationId.create" should "generate reconciliationId based on submissionRef and current time" in {

    val rnd = new scala.util.Random(123)

    implicit val now = Now(LocalDateTime.of(2017, 1, 31, 13, 53, 45))

    val submissionRef = SubmissionRef.createSubmissionRef(rnd)

    val res = ReconciliationId.create(submissionRef)

    res.value should be("0OU-RDFS-NRN-20170131135345")

  }
}
