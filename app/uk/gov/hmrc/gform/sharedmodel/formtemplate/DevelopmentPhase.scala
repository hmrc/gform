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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json._

sealed trait DevelopmentPhase {
  val banner: String
}
case object AlphaBanner extends DevelopmentPhase {
  override val banner: String = "alpha"
}
case object BetaBanner extends DevelopmentPhase {
  override val banner: String = "beta"
}
case object ResearchBanner extends DevelopmentPhase {
  override val banner: String = "research"
}
case object LiveBanner extends DevelopmentPhase {
  override val banner: String = "live"
}
object DevelopmentPhase {
  implicit val format: Format[DevelopmentPhase] = new Format[DevelopmentPhase] {
    override def writes(o: DevelopmentPhase): JsValue = o match {
      case AlphaBanner    => JsString(AlphaBanner.banner)
      case BetaBanner     => JsString(BetaBanner.banner)
      case ResearchBanner => JsString(ResearchBanner.banner)
      case LiveBanner     => JsString(LiveBanner.banner)
    }

    override def reads(json: JsValue): JsResult[DevelopmentPhase] =
      json match {
        case JsString("alpha")    => JsSuccess(AlphaBanner)
        case JsString("beta")     => JsSuccess(BetaBanner)
        case JsString("research") => JsSuccess(ResearchBanner)
        case JsString("live")     => JsSuccess(LiveBanner)
        case JsString(err) =>
          JsError(s"only folr valid DevelopmentPhase, alpha, beta, research or live.$err is not allowed")
        case _ => JsError("Failure")
      }
  }
}
