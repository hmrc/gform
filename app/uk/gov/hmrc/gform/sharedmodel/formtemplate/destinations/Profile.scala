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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OFormatWithTemplateReadFallback

sealed trait Profile extends Product with Serializable

object Profile {
  case object DES extends Profile
  case object MdgIntegrationFramework extends Profile
  case class MDTP(serviceName: String) extends Profile

  implicit val format: OFormat[Profile] = {
    val mdtpUploadReads = new Reads[Profile] {
      private val extractor = """([mM][dD][tT][pP])(\.)([a-zA-Z_][a-zA-Z0-9_-]*)""".r

      override def reads(json: JsValue): JsResult[Profile] = json match {
        case JsString("des")                        => JsSuccess(DES)
        case JsString("mdgIntegrationFramework")    => JsSuccess(MdgIntegrationFramework)
        case JsString(extractor(_, _, serviceName)) => JsSuccess(MDTP(serviceName))
        case _ =>
          JsError(s"""Profile must be one of "des", "mdgIntegrationFramework", "mdtp.<servicename>". Got: $json""")
      }
    }

    OFormatWithTemplateReadFallback(mdtpUploadReads)
  }
}
