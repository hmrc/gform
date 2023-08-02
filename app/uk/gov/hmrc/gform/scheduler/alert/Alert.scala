/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.scheduler.alert

import julienrf.json.derived
import play.api.libs.json.{ Format, OFormat }
import uk.gov.hmrc.gform.sharedmodel.email.EmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ADTFormat, EmailParametersRecalculated }
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress

import java.time.Instant

case class Alert(
  name: AlertName,
  method: AlertMethod,
  notifierEmailAddress: NotifierEmailAddress,
  emailTemplateId: EmailTemplateId,
  emailParameters: EmailParametersRecalculated,
  createdAt: Instant = Instant.now
)

object Alert {
  implicit val format: OFormat[Alert] = derived.oformat()
}

sealed trait AlertName
object AlertName {
  case object SdesAlert extends AlertName
  implicit val format: Format[AlertName] =
    ADTFormat.formatEnumeration(
      "SdesAlert" -> SdesAlert
    )
}
sealed trait AlertMethod
object AlertMethod {
  case object Email extends AlertMethod
  implicit val format: Format[AlertMethod] =
    ADTFormat.formatEnumeration(
      "Email" -> Email
    )
}
