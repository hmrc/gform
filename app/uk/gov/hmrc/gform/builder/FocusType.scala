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

package uk.gov.hmrc.gform.builder

import io.circe.Decoder

sealed trait FocusType extends Product with Serializable

object FocusType {
  case object TaskSection extends FocusType
  case object TaskTitle extends FocusType
  case object TaskCaption extends FocusType
  case object TaskSummarySection extends FocusType
  case object SubmitSection extends FocusType

  private def parse(s: String): Option[FocusType] =
    s match {
      case "taskSection"        => Some(TaskSection)
      case "taskTitle"          => Some(TaskTitle)
      case "taskCaption"        => Some(TaskCaption)
      case "taskSummarySection" => Some(TaskSummarySection)
      case "submitSection"      => Some(SubmitSection)
      case _                    => None
    }

  implicit val focusTypeDecoder: Decoder[FocusType] = implicitly[Decoder[String]].emap { str =>
    parse(str).fold[Either[String, FocusType]](Left(s"Invalid focusType: $str"))(Right(_))
  }
}
