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

package uk.gov.hmrc.gform.dms

import java.nio.file.Files.readAllBytes
import java.nio.file.Paths

import play.api.libs.Files.TemporaryFile
import play.api.libs.json.{ JsError, JsSuccess, Json }
import play.api.mvc.{ MultipartFormData, Request }

object DmsSubmissionWithAttachmentsRequestInterpreter {
  def apply(
    request: Request[MultipartFormData[TemporaryFile]]): Either[String, (String, List[FileAttachment], DmsMetadata)] = {

    val maybeHtml: Option[Seq[String]] = request.body.dataParts.get("html")
    val maybeMetadata: Option[DmsMetadata] = Json
      .toJson(request.body.dataParts.filterKeys(_ == "html").mapValues(_.mkString("")))
      .validate[DmsMetadata] match {
      case JsSuccess(metadata: DmsMetadata, _) => Some(metadata)
      case JsError(_)                          => None
    }

    val maybeFiles: List[FileAttachment] = request.body.files.map { file =>
      {
        val filename = Paths.get(file.filename).getFileName
        val bytes = readAllBytes(file.ref.moveTo(Paths.get(s"/tmp/dms/$filename"), replace = true))
        val contentType = file.contentType
        FileAttachment(filename, bytes, contentType)
      }
    }.toList

    (maybeHtml, maybeMetadata) match {
      case (Some(html), Some(meta)) => Right((html.mkString(""), maybeFiles, meta))
      case (None, _)                => Left("request should contain html")
      case (_, None)                => Left("request should contain DMS meta data")
      case (_, _)                   => Left("request should contain html and DMS meta data")
    }
  }
}
