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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json._

sealed trait ServiceCallResponse[+A] extends Product with Serializable

case object NotFound extends ServiceCallResponse[Nothing]
case object CannotRetrieveResponse extends ServiceCallResponse[Nothing]
case class ServiceResponse[A](value: A) extends ServiceCallResponse[A]

object ServiceCallResponse {
  implicit def format[A: OWrites: Reads]: OFormat[ServiceCallResponse[A]] = new OFormat[ServiceCallResponse[A]] {
    override def writes(o: ServiceCallResponse[A]): JsObject =
      o match {
        case NotFound               => Json.obj("det" -> "NotFound")
        case CannotRetrieveResponse => Json.obj("det" -> "CannotRetrieveResponse")
        case ServiceResponse(a)     => Json.obj("det" -> implicitly[OWrites[A]].writes(a))
      }

    override def reads(json: JsValue): JsResult[ServiceCallResponse[A]] =
      json \ "det" match {
        case JsDefined(js) =>
          js match {
            case JsString("NotFound")               => JsSuccess(NotFound: ServiceCallResponse[A])
            case JsString("CannotRetrieveResponse") => JsSuccess(CannotRetrieveResponse: ServiceCallResponse[A])
            case jsObject: JsObject                 => implicitly[Reads[A]].reads(jsObject).map(ServiceResponse.apply)
            case _                                  => JsError("Not Supported json: " + Json.prettyPrint(json))
          }

        case JsUndefined() => JsError("Not Supported json: " + Json.prettyPrint(json))
      }
  }
}
