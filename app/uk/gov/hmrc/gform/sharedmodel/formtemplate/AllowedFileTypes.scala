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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.{ NonEmptyList, Validated }
import cats.implicits._
import play.api.libs.json.{ JsArray, JsError, JsString, JsSuccess, JsValue, OFormat, Reads }
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.sharedmodel.config.ContentType

final case class AllowedFileTypes(fileExtensions: NonEmptyList[String]) {

  val allExtensions: Set[String] = fileExtensions.toList.toSet

  val contentTypes: NonEmptyList[ContentType] = FileInfoConfig.contentTypes(fileExtensions)

  def contains(s: String): Boolean = allExtensions(s)
}

object AllowedFileTypes extends JsonUtils {
  private val templateReads: Reads[AllowedFileTypes] = Reads {
    case JsArray(types) =>
      val (uncheckedFileExtensionsJson, invalid): (IndexedSeq[JsValue], IndexedSeq[JsValue]) =
        types.partition {
          case JsString(_) => true
          case _           => false
        }

      val uncheckedFileExtensions: IndexedSeq[String] = uncheckedFileExtensionsJson.collect { case JsString(s) =>
        s
      }
      val (allowedFileExtensions, unsupportedFileExtension) = uncheckedFileExtensions.partition {
        case extension if FileInfoConfig.allAllowedFileTypes.contains(extension) => true
        case _                                                                   => false
      }

      Validated
        .fromOption(allowedFileExtensions.toList.toNel, "Empty 'allowedFileTypes' is not permitted.")
        .ensure("Invalid 'allowedFileTypes' values found: " + invalid.mkString(", "))(_ => invalid.isEmpty)
        .ensure(
          "Unsupported extension(s) in 'allowedFileTypes' found: " + unsupportedFileExtension.mkString(
            ", "
          ) + ", supported file extension are: " + FileInfoConfig.allAllowedFileTypes.allExtensions.mkString(", ")
        )(_ => unsupportedFileExtension.isEmpty)
        .bimap(fe => JsError(fe), nel => JsSuccess(AllowedFileTypes(nel)))
        .merge

    case invalid => JsError("Invalid 'allowedFileTypes'. Expected array of strings, got: " + invalid)
  }
  implicit val format: OFormat[AllowedFileTypes] = OFormatWithTemplateReadFallback(templateReads)
}
