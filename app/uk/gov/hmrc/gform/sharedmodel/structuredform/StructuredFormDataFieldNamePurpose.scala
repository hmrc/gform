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

package uk.gov.hmrc.gform.sharedmodel.structuredform
import play.api.libs.json.Format
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ADTFormat, FormComponentId }

trait StructuredFormDataFieldNamePurpose
case object RoboticsXml extends StructuredFormDataFieldNamePurpose {

  def replaceStreetWithLine(fcId: FormComponentId): FieldName = {
    val r = "street[1-4]".r
    val value = fcId.value

    if (r.findFirstIn(value).isDefined) FieldName(value.replace("street", "line"))
    else FieldName(value)
  }
}

object StructuredFormDataFieldNamePurpose {
  val roboticsXml = "RoboticsXml"

  implicit val format: Format[StructuredFormDataFieldNamePurpose] =
    ADTFormat.formatEnumeration(roboticsXml -> RoboticsXml)
}
