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

package uk.gov.hmrc.gform.sdes

import scala.concurrent.duration.Duration

final case class SdesConfig(
  basePath: String,
  fileLocationUrl: String,
  dms: SdesRouting,
  hmrcIlluminate: SdesRouting,
  dataStore: SdesRouting,
  lockTTL: Long
)

final case class SdesRouting(apiKey: String, informationType: String, recipientOrSender: String)

final case class SdesAlertConfig(
  enabled: Boolean,
  cron: String,
  destination: Option[List[String]],
  notifierEmailAddress: String,
  emailTemplateId: String,
  lockDuration: Duration
)

final case class SdesRenotifyConfig(
  enabled: Boolean,
  cron: String,
  destinations: List[String],
  showBeforeSubmittedAt: Int,
  lockDuration: Duration
)
