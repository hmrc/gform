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

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import JsonUtils._

sealed trait Destinations extends Product with Serializable

object Destinations {
  // This is for handling the case of the deprecated FormTemplate.dmsSubmission field.
  // In the FormTemplate template upload Reads, we will take the content of the dmsSubmission field
  // and put it here.
  case class DmsSubmission(
    dmsFormId: String,
    customerId: TextExpression,
    classificationType: String,
    businessArea: String,
    dataXml: Option[Boolean] = None)
      extends Destinations

  implicit val dmsSubmissionFormat: OFormat[DmsSubmission] = Json.format

  case class DestinationList(destinations: NonEmptyList[Destination]) extends Destinations

  implicit val destinationListReads: OFormat[DestinationList] = derived.oformat[DestinationList]

  implicit val format: OFormat[Destinations] = {
    implicit val destinationsFormat: OFormat[Destinations] = {
      implicit val _ = derived.reads[DmsSubmission]
      derived.oformat
    }

    OFormatWithTemplateReadFallback(
      // When uploading the template and loading a Destinations, we can only read the DestinationList branch.
      // See the comment above DmsSubmission.
      JsonUtils.valueClassReads[Destinations, NonEmptyList[Destination]](DestinationList))
  }
}
