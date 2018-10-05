/*
 * Copyright 2018 HM Revenue & Customs
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

sealed trait DraftRetrievalMethod

case object OnePerUser extends DraftRetrievalMethod
case object FormAccessCodeForAgents extends DraftRetrievalMethod

object DraftRetrievalMethod {
  implicit val format: Format[DraftRetrievalMethod] = new Format[DraftRetrievalMethod] {
    override def writes(o: DraftRetrievalMethod): JsValue = o match {
      case OnePerUser              => JsString("onePerUser")
      case FormAccessCodeForAgents => JsString("formAccessCodeForAgents")
    }

    override def reads(json: JsValue): JsResult[DraftRetrievalMethod] =
      json match {
        case JsString("onePerUser") =>
          JsSuccess(OnePerUser)
        case JsString("formAccessCodeForAgents") =>
          JsSuccess(FormAccessCodeForAgents)
        case JsString(err) =>
          JsError(s"only two valid draft retrieval methods: onePerUser, or formAccessCodeForAgents; $err is not valid")
        case _ => JsError("Failure")
      }
  }
}
