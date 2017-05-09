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

import cats.data.State
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import play.api.libs.json.{ Format, JsError, JsString, JsSuccess, Reads, Writes }
import scala.util.Random
import uk.gov.hmrc.gform.typeclasses.Now

case class ReconciliationId(value: String) extends AnyVal {
  override def toString = value
}

object ReconciliationId {

  def create(submissionRef: SubmissionRef)(implicit now: Now[LocalDateTime]): ReconciliationId = {
    val dateFormatter = now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
    ReconciliationId(submissionRef + "-" + dateFormatter)
  }
}
