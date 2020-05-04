/*
 * Copyright 2020 HM Revenue & Customs
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

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, JsonUtils }
import uk.gov.hmrc.gform.sharedmodel.{ NotChecked, Obligations }
import uk.gov.hmrc.gform.sharedmodel.des.DesRegistrationResponse

case class ThirdPartyData(
  desRegistrationResponse: Option[DesRegistrationResponse],
  obligations: Obligations,
  emailVerification: Map[FormComponentId, EmailAndCode],
  queryParams: QueryParams,
  reviewData: Option[Map[String, String]] = None
) {

  def reviewComments: Option[String] = reviewData.flatMap(_.get("caseworkerComment"))
}

object ThirdPartyData {
  val empty = ThirdPartyData(None, NotChecked, Map.empty, QueryParams.empty)

  // This will be safe to remove one month after release of this reads,
  // because at that point all forms in mongo will have 'queryParams' field.
  // Forms are kept only 28 days in Save4Later.
  val readsWithDefaultQueryParams: Reads[ThirdPartyData] = Reads { json =>
    val thirdPartyData: JsLookupResult = (json \ "thirdPartyData")
    val thirdPartyDataUpd = thirdPartyData match {
      case JsDefined(obj: JsObject) => JsDefined(obj ++ Json.obj("queryParams" -> Json.obj("params" -> Json.obj())))
      case otherwise                => otherwise
    }

    thirdPartyDataUpd
      .asOpt[ThirdPartyData]
      .fold[JsResult[ThirdPartyData]](JsError(s"Failed to read $thirdPartyDataUpd as ThirdPartyData"))(JsSuccess(_))
  }

  implicit val formatMap: Format[Map[FormComponentId, EmailAndCode]] =
    JsonUtils.formatMap(FormComponentId.apply, _.value)
  implicit val format: OFormat[ThirdPartyData] = Json.format
}
