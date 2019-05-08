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

package uk.gov.hmrc.gform.submission

import uk.gov.hmrc.gform.sharedmodel.LocalisedString
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileUpload, FormComponent }

import scala.collection.immutable.List

object FileUploadField {
  val noFileUpload = "Upload document"
}

case class SectionFormField(title: LocalisedString, fields: List[(List[FormField], FormComponent)]) {

  // TODO two functions are calculating the same thing in different ways! c.f. SubmissionService.getNoOfAttachments
  def numberOfFiles(): Int =
    fields
      .filter(_ match {
        case (FormField(_, "") :: xs, _) => false
        case (
            FormField(_, FileUploadField.noFileUpload) :: xs,
            FormComponent(_, FileUpload(), _, _, _, _, _, _, _, _, _, _, _)) =>
          false
        case (x :: xs, FormComponent(_, FileUpload(), _, _, _, _, _, _, _, _, _, _, _)) => true
        case _                                                                          => false
      })
      .size

}
