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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.json.{ Json, OFormat, OWrites, Reads }
import uk.gov.hmrc.gform.sharedmodel.Obligations
import play.api.libs.functional.syntax._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.EmailParametersRecalculated

case class UserData(
  formData: FormData,
  formStatus: FormStatus,
  visitsIndex: VisitIndex,
  thirdPartyData: ThirdPartyData,
  obligations: Obligations
)

object UserData {

  private val reads: Reads[UserData] = (
    (FormData.format: Reads[FormData]) and
      FormStatus.format and
      VisitIndex.format and
      ThirdPartyData.format and
      Obligations.format
  )(UserData.apply _)

  private val writes: OWrites[UserData] = OWrites[UserData](
    userData =>
      FormData.format.writes(userData.formData) ++
        FormStatus.format.writes(userData.formStatus) ++
        VisitIndex.format.writes(userData.visitsIndex) ++
        ThirdPartyData.format.writes(userData.thirdPartyData) ++
        Obligations.format.writes(userData.obligations)
  )

  implicit val format: OFormat[UserData] = OFormat[UserData](reads, writes)

}
