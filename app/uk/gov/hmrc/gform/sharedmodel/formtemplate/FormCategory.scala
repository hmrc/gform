/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.LangADT

sealed trait FormCategory {
  def localised(l: LangADT): String
  def isHmrc: Boolean = this != OfstedDefault
}
case object HMRCReturnForm extends FormCategory {
  override def localised(l: LangADT): String = l match {
    case LangADT.En => "return"
    case LangADT.Cy => "datganiad"
  }
}
case object HMRCClaimForm extends FormCategory {
  override def localised(l: LangADT): String = l match {
    case LangADT.En => "claim"
    case LangADT.Cy => "hawliad"
  }
}
case object OfstedDefault extends FormCategory {
  override def localised(l: LangADT): String = l match {
    case LangADT.En => "form"
    case LangADT.Cy => "ffurflen"
  }
}
case object Default extends FormCategory {
  override def localised(l: LangADT): String = l match {
    case LangADT.En => "form"
    case LangADT.Cy => "ffurflen"
  }
}

object FormCategory {
  implicit val format: Format[FormCategory] = new Format[FormCategory] {
    override def writes(o: FormCategory): JsValue = o match {
      case HMRCReturnForm => JsString("hmrcReturnForm")
      case HMRCClaimForm  => JsString("hmrcClaimForm")
      case OfstedDefault  => JsString("ofstedDefault")
      case Default        => JsString("default")
    }

    override def reads(json: JsValue): JsResult[FormCategory] =
      json match {
        case JsString("hmrcReturnForm") => JsSuccess(HMRCReturnForm)
        case JsString("hmrcClaimForm")  => JsSuccess(HMRCClaimForm)
        case JsString("ofstedDefault")  => JsSuccess(OfstedDefault)
        case JsString("default")        => JsSuccess(Default)
        case JsString(err) =>
          JsError(
            s"only four valid categories, hmrcReturnForm, hmrcClaimForm, ofstedDefault or default $err is not valid"
          )
        case _ => JsError("Failure")
      }
  }
}
