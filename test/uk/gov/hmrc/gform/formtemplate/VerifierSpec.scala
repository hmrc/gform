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

package uk.gov.hmrc.gform.formtemplate

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Seconds, Span }
import org.scalatest.{ Matchers, WordSpecLike }
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BasicText, FormComponent, FormComponentId, Instruction, Page, Section, Text, Value }

import scala.concurrent.ExecutionContext.Implicits.global

class VerifierSpec extends WordSpecLike with Matchers with ScalaFutures {

  implicit val defaultPatience = PatienceConfig(timeout = Span(6, Seconds), interval = Span(5, Millis))

  "verify" should {
    "validate instructions" in {
      val sections = List(
        mkSection(
          name = "section1",
          formComponents = List(
            mkFormComponent("section1Component1", Some(Instruction(toSmartString("section1Component1Instruction"), Some(1))))
          ),
          Some(Instruction(toSmartString(""), Some(1)))
        ),
      )

      val result = Verifier.verify(mkFormTemplate(sections))

      result.value.futureValue shouldBe Left(
        UnexpectedState("One or more sections have instruction attribute with empty names"))
    }
  }

  private def mkFormTemplate(sections: List[Section.NonRepeatingPage]) = {
    val formTemplate = ExampleData.formTemplate.copy(sections = sections, emailParameters = None)
    formTemplate
  }

  private def mkFormComponent(id: String, instruction: Option[Instruction]) =
    FormComponent(
      FormComponentId(id),
      Text(BasicText, Value),
      toSmartString(id),
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None,
      Nil,
      instruction
    )

  private def mkSection(name: String, formComponents: List[FormComponent], instruction: Option[Instruction]) =
    Section.NonRepeatingPage(
      Page(
        toSmartString(name),
        None,
        None,
        None,
        None,
        None,
        formComponents,
        None,
        None,
        instruction
      ))
}
