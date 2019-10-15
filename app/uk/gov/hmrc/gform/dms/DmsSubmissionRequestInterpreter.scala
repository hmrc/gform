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

package uk.gov.hmrc.gform.dms

import java.nio.file.Files.readAllBytes

import play.api.libs.Files.TemporaryFile
import play.api.libs.json.{ JsError, JsSuccess, Json }
import play.api.mvc.{ MultipartFormData, Request }

object DmsSubmissionRequestInterpreter {
  def apply(request: Request[MultipartFormData[TemporaryFile]]): Either[String, (Array[Byte], DmsMetadata)] =
    request.body.files.headOption match {
      case Some(file) if validContentType(file) =>
        val dataParts = request.body.dataParts.mapValues(_.mkString(""))
        Json.toJson(dataParts).validate[DmsMetadata] match {
          case JsSuccess(metadata: DmsMetadata, _) => Right((readAllBytes(file.ref.path), metadata))
          case JsError(errors)                     => Left(s"Invalid metadata in the request. errors: $errors")
        }
      case _ => Left("request should contain a pdf file with Content-Type:'application/pdf'")
    }

  def validContentType(filePart: MultipartFormData.FilePart[TemporaryFile]) =
    filePart.contentType.map(_.toLowerCase).contains("application/pdf")
}
