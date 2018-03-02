/*
 * Copyright 2018 HM Revenue & Customs
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

import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileUpload, FormComponent }

import scala.collection.immutable.List

case class SectionFormField(
  title: String,
  fields: List[(List[FormField], FormComponent)]) {

  private def fileUploadNonEmpty(fieldComponent: (List[FormField], FormComponent)): Boolean = {
    fieldComponent._2.`type` match {
      case FileUpload() => {
        fieldComponent._1 match {
          case x :: xs => !x.value.isEmpty
          case _ => false
        }
      }
      case _ => false
    }
  }

  def numberOfFiles(): Int = fields.filter(fileUploadNonEmpty(_)).size

}
