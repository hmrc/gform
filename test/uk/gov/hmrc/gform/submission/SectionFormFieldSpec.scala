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

import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId }

import scala.collection.immutable.List

class SectionFormFieldSpec extends Spec with ExampleData {

  val fileField: (List[FormField], FormComponent) = (List(`formField - facePhoto`), `fieldValue - facePhoto`)
  val noFileField: (List[FormField], FormComponent) =
    (List(FormField(FormComponentId("1"), "")), `fieldValue - facePhoto`)
  val emptyFileField: (List[FormField], FormComponent) = (List(), `fieldValue - facePhoto`)
  val textField: (List[FormField], FormComponent) = (List(`formField - surname`), `fieldValue - surname`)

  "numberOfFiles" should "return zero when only text present" in new ExampleData {

    val s = SectionFormField(toLocalisedString(""), List(textField))
    s.numberOfFiles() shouldBe 0

  }

  "numberOfFiles" should "return zero when only empty file present" in new ExampleData {

    val s = SectionFormField(toLocalisedString(""), List(emptyFileField))
    s.numberOfFiles() shouldBe 0

  }

  "numberOfFiles" should "return zero when no filename file present" in new ExampleData {

    val s = SectionFormField(toLocalisedString(""), List(noFileField))
    s.numberOfFiles() shouldBe 0

  }

  "numberOfFiles" should "return zero when no file present" in new ExampleData {

    val s = SectionFormField(toLocalisedString(""), List(noFileField, emptyFileField, textField))
    s.numberOfFiles() shouldBe 0

  }

  "numberOfFiles" should "return one when one file present" in new ExampleData {

    val s = SectionFormField(toLocalisedString(""), List(fileField, noFileField, emptyFileField, textField))
    s.numberOfFiles() shouldBe 1

  }

}
