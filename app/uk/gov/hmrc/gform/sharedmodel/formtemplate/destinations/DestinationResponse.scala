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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import julienrf.json.derived
import org.bson.types.ObjectId
import play.api.libs.json.{ Format, JsError, JsNull, JsString, JsSuccess, JsValue, Reads, Writes }
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination
import uk.gov.hmrc.http.HttpResponse

import scala.util.Try

sealed trait DestinationResponse

object DestinationResponse {
  val objectIdReads: Reads[ObjectId] = Reads {
    case JsString(s) =>
      Try(new ObjectId(s)).fold(
        _ => JsError(s"invalid ObjectId string: $s"),
        oid => JsSuccess(oid)
      )
    case unexpected => JsError(s"expected JSON string for ObjectId, got: $unexpected")
  }

  val objectIdWrites: Writes[ObjectId] = Writes(oid => JsString(oid.toHexString))

  implicit val objectIdFormat: Format[ObjectId] = Format(objectIdReads, objectIdWrites)

  case object NoResponse extends DestinationResponse
}

case class DmsDestinationResponse(
  dmsFormId: String,
  routing: SdesDestination,
  classificationType: String,
  businessArea: String,
  dataItemCount: Int,
  workItemId: ObjectId
) extends DestinationResponse

object DmsDestinationResponse {
  import DestinationResponse._
  implicit val format: Format[DmsDestinationResponse] = derived.oformat()
}

case class OtherSdesDestinationResponse(
  routing: SdesDestination,
  workItemId: ObjectId
) extends DestinationResponse

object OtherSdesDestinationResponse {
  import DestinationResponse._
  implicit val format: Format[OtherSdesDestinationResponse] = derived.oformat()
}

case class HandlebarsDestinationResponse(id: DestinationId, status: Int, json: JsValue) extends DestinationResponse

object HandlebarsDestinationResponse {
  def apply(destination: Destination.HandlebarsHttpApi, response: HttpResponse): HandlebarsDestinationResponse =
    HandlebarsDestinationResponse(destination.id, response.status, responseJson(response))

  private def responseJson(response: HttpResponse): JsValue =
    try Option(response.json) getOrElse JsNull
    catch {
      case _: Exception => JsNull
    }
}
