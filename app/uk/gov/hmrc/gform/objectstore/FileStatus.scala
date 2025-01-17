/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.objectstore

import play.api.libs.json.{ Format, JsPath, JsString, Reads, Writes }

import scala.annotation.nowarn

sealed trait FileStatus {
  val value: String
}

object FileStatus {
  case object Available extends FileStatus {
    val value = "AVAILABLE"
  }

  def unapply(status: FileStatus): String = status.value

  @nowarn
  val reads: Reads[FileStatus] = for {
    value <- JsPath.read[String].map { case FileStatus.Available.value =>
               FileStatus.Available
             }
  } yield value

  val writes: Writes[FileStatus] = Writes { status: FileStatus =>
    JsString(status.value)
  }

  implicit val format: Format[FileStatus] = Format(reads, writes)
}
