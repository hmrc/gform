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

import io.circe.{ Json, parser }
import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.sharedmodel.{ DataStoreMetaData, UserSession }

object DataStoreFileGenerator {
  def apply(userSession: UserSession, metaData: DataStoreMetaData, gform: String) = {

    def playJsonToCirce(jsValue: JsValue): Json = {
      val jsonString = play.api.libs.json.Json.stringify(play.api.libs.json.Json.toJson(jsValue))
      io.circe.parser.parse(jsonString).getOrElse(throw new RuntimeException("Failed to parse enrolments JSON"))
    }

    val metaDataJson: Json = Json.obj(
      "formId"         -> Json.fromString(metaData.formId),
      "version"        -> Json.fromString(metaData.version),
      "mdtpXrefId"     -> Json.fromString(""),
      "regime"         -> Json.fromString(metaData.regime),
      "taxpayerId"     -> Json.fromString(metaData.taxpayerId),
      "submissionDate" -> Json.fromString(metaData.submissionDate),
      "submissionTime" -> Json.fromString(metaData.submissionTime)
    )

    val userSessionJson: Json = Json.obj(
      "clientIp"      -> Json.fromString(userSession.clientIp),
      "deviceId"      -> Json.fromString(userSession.deviceId),
      "userAgent"     -> Json.fromString(userSession.userAgent),
      "relativePath"  -> Json.fromString(userSession.relativePath),
      "credentialId"  -> Json.fromString(userSession.credentialId),
      "affinityGroup" -> Json.fromString(userSession.affinityGroup.map(_.toString).getOrElse("")),
      "authEmail"     -> Json.fromString(userSession.authEmail),
      "authPhone"     -> Json.fromString(userSession.authPhone),
      "enrolments"    -> Json.arr(userSession.enrolments.map(playJsonToCirce): _*)
    )

    val gformJson: Json = parser.parse(gform).getOrElse(throw new RuntimeException("Failed to parse gform JSON"))
    val resultJson = Json.obj(
      "metaData"    -> metaDataJson,
      "userSession" -> userSessionJson
    )
    gformJson.deepMerge(resultJson).noSpaces
  }
}
