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

sealed trait PhaseBanner {
  val banner: String
}
case object AlphaBanner extends PhaseBanner {
  override val banner: String = "alpha"
}
case object BetaBanner extends PhaseBanner {
  override val banner: String = "beta"
}
case object LiveBanner extends PhaseBanner {
  override val banner: String = "live"
}
case object DiscoveryBanner extends PhaseBanner {
  override val banner: String = "discovery"
}
object PhaseBanner {
  implicit val format: Format[PhaseBanner] = new Format[PhaseBanner] {
    override def writes(o: PhaseBanner): JsValue = o match {
      case AlphaBanner     => JsString(AlphaBanner.banner)
      case BetaBanner      => JsString(BetaBanner.banner)
      case LiveBanner      => JsString(LiveBanner.banner)
      case DiscoveryBanner => JsString(DiscoveryBanner.banner)
    }

    override def reads(json: JsValue): JsResult[PhaseBanner] =
      json match {
        case JsString("alpha")     => JsSuccess(AlphaBanner)
        case JsString("beta")      => JsSuccess(BetaBanner)
        case JsString("live")      => JsSuccess(LiveBanner)
        case JsString("discovery") => JsSuccess(DiscoveryBanner)
        case JsString(err) =>
          JsError(
            s"only valid PhaseBanner values are alpha, beta, live or discovery; $err is not allowed"
          )
        case _ => JsError("Failure")
      }
  }
}
