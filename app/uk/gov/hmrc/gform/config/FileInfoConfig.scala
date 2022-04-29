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

package uk.gov.hmrc.gform.config

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AllowedFileTypes

object FileInfoConfig {
  private val allowedFileInfo: NonEmptyList[(String, ContentType)] = NonEmptyList
    .of(
      ("pdf", "application/pdf"),
      ("jpg", "image/jpeg"),
      ("xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
      ("ods", "application/vnd.oasis.opendocument.spreadsheet"),
      ("docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document"),
      ("odt", "application/vnd.oasis.opendocument.text"),
      ("pptx", "application/vnd.openxmlformats-officedocument.presentationml.presentation"),
      ("odp", "application/vnd.oasis.opendocument.presentation")
    )
    .map { case (extension, mimeType) => extension -> ContentType(mimeType) }

  private val lookup: Map[String, ContentType] = allowedFileInfo.toList.toMap

  def contentTypes(fileExtensions: NonEmptyList[String]): NonEmptyList[ContentType] = fileExtensions.map(lookup)

  val allAllowedFileTypes: AllowedFileTypes = AllowedFileTypes(allowedFileInfo.map { case (extension, _) => extension })
}
