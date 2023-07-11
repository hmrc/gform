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

package uk.gov.hmrc.gform.submission

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ DataStoreMetaData, UserSession }

object DataStoreFileGenerator {
  def apply(
    userSession: UserSession,
    metaData: DataStoreMetaData,
    gform: String,
    includeSessionInfo: Boolean
  ): String = {

    val metaDataJson: JsObject = Json.obj(
      "formId"         -> JsString(metaData.formId),
      "version"        -> JsString(metaData.version),
      "mdtpXrefId"     -> JsString(""),
      "regime"         -> JsString(metaData.regime),
      "taxpayerId"     -> JsString(metaData.taxpayerId),
      "submissionDate" -> JsString(metaData.submissionDate),
      "submissionTime" -> JsString(metaData.submissionTime)
    )

    val gformJson: JsValue = Json.parse(gform)

    val dataStoreJson: JsObject = if (includeSessionInfo) {
      val userSessionJson: JsObject = Json.obj(
        "clientIp"      -> JsString(userSession.clientIp),
        "deviceId"      -> JsString(userSession.deviceId),
        "userAgent"     -> JsString(userSession.userAgent),
        "relativePath"  -> JsString(userSession.relativePath),
        "credentialId"  -> JsString(userSession.credentialId),
        "affinityGroup" -> JsString(userSession.affinityGroup.map(_.toString).getOrElse("")),
        "authEmail"     -> JsString(userSession.authEmail),
        "authPhone"     -> JsString(userSession.authPhone),
        "enrolments"    -> JsArray(userSession.enrolments.map(Json.toJson(_)))
      )

      Json.obj(
        "metaData"    -> metaDataJson,
        "userSession" -> userSessionJson
      )
    } else {
      Json.obj("metaData" -> metaDataJson)
    }

    (dataStoreJson ++ gformJson.as[JsObject]).toString()
  }
}
