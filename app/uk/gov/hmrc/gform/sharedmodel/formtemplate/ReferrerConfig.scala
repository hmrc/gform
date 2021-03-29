/*
 * Copyright 2021 HM Revenue & Customs
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

import play.api.libs.json.{ Format, Json }

case class ReferrerConfig(allowedReferrerUrls: List[ReferrerUrlPattern], exitMessage: String) {
  def isAllowed(referrer: String): Boolean =
    if (allowedReferrerUrls.isEmpty)
      true
    else
      allowedReferrerUrls.exists(a => patternMatch(a.urlPattern, referrer))

  private def patternMatch(pattern: String, source: String): Boolean =
    pattern.replace("*", ".+").r.findFirstIn(source).isDefined
}

object ReferrerConfig {
  implicit val format: Format[ReferrerConfig] = Json.format[ReferrerConfig]
}
