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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat

case class AuthConfig(
  authModule: AuthConfigModule,
  predicates: Option[List[Predicate]],
  regimeId: RegimeId,
  serviceId: Option[ServiceId]
)

object AuthConfig {

  implicit val format: OFormat[AuthConfig] = Json.format[AuthConfig]
}

case class ServiceId(value: String)

object ServiceId {
  implicit val format: OFormat[ServiceId] = ValueClassFormat.oformat("value", ServiceId.apply, _.value)
}

case class RegimeId(value: String) {
  override def toString: String = value
}

object RegimeId {

  implicit val format: Format[RegimeId] = ValueClassFormat.oformat("regimeId", RegimeId.apply, _.value)
}

case class AuthConfigModule(value: String) {
  override def toString: String = value
}

object AuthConfigModule {

  val legacyEEITTAuth = "legacyEEITTAuth"
  implicit val format: Format[AuthConfigModule] = ValueClassFormat.oformat("authModule", AuthConfigModule.apply, _.value)
}

case class Predicate(enrolment: String, identifiers: List[KeyValue], delegatedAuthRule: String)

object Predicate {
  implicit val format: OFormat[Predicate] = Json.format[Predicate]
}

case class KeyValue(key: String, value: String)

object KeyValue {
  implicit val format: OFormat[KeyValue] = Json.format[KeyValue]
}