/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.LangADT.{ Cy, En }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OFormatWithTemplateReadFallback

sealed trait LangADT {
  def langADTToString: String = this match {
    case Cy => "cy"
    case En => "en"
  }
}

object LangADT {
  case object En extends LangADT
  case object Cy extends LangADT

  def langADTToString(langADT: LangADT): String = langADT.langADTToString

  def stringToLangADT(string: String): LangADT = string match {
    case "cy" => Cy
    case _    => En
  }

  private def convertToLang(jsValue: JsValue): JsResult[LangADT] = {
    println("jsValue " + jsValue)
    jsValue match {
      case JsString(message) =>
        message match {
          case "en" => JsSuccess(LangADT.En)
          case "cy" => JsSuccess(LangADT.Cy)
          case l    => JsError("Unsupported language " + l)
        }
      case _ => JsError("Expected Lang, got " + jsValue)
    }
  }
  implicit val langADTReads: Reads[LangADT] = Reads.apply[LangADT](convertToLang)
  implicit val format: OFormat[LangADT] = OFormatWithTemplateReadFallback(langADTReads)

}
