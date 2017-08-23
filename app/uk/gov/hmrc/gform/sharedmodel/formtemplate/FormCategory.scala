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

trait FormCategory
object HMRCReturnForm extends FormCategory
object HMRCClaimForm extends FormCategory
object Default extends FormCategory

object FormCategory {

  implicit val format: Format[FormCategory] = new Format[FormCategory] {
    override def writes(o: FormCategory): JsValue = o match {
      case HMRCReturnForm => JsString("hmrcReturnForm")
      case HMRCClaimForm => JsString("hmrcClaimForm")
      case Default => JsString("default")
    }

    override def reads(json: JsValue): JsResult[FormCategory] = {
      json match {
        case JsString("hmrcReturnForm") => JsSuccess(HMRCReturnForm)
        case JsString("hmrcClaimForm") => JsSuccess(HMRCClaimForm)
        case JsString("default") => JsSuccess(Default)
        case JsString(err) => JsError(s"only two valid categories, hmrcReturnForm or hmrcClaimForm $err is not valid")
        case _ => JsError("Failure")
      }
    }
  }
}
