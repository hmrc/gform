/*
 * Copyright 2021 HM Revenue & Customs
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

import cats.data.NonEmptyList
import munit.FunSuite
import scala.concurrent.Future
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class RewriterSpec extends FunSuite with FormTemplateSupport {

  val rewriter = new Rewriter {}

  test("Rewrite includeIf when Equals refers to Choice component") {

    val yesNoLocalisedStrings = NonEmptyList.of(toSmartString("Yes"), toSmartString("No"))

    val choice = Choice(YesNo, yesNoLocalisedStrings, Horizontal, Nil, None, None)

    val fcChoice = mkFormComponent("a", choice, false)
    val fcText = mkFormComponent("b", Value)

    val fcTextWithIncludeIf =
      fcText.copy(includeIf = Some(IncludeIf(Equals(FormCtx(FormComponentId("a")), Constant("1")))))

    val fcTextExpected =
      fcText.copy(includeIf = Some(IncludeIf(Contains(FormCtx(FormComponentId("a")), Constant("1")))))

    val sections = List(
      mkSectionNonRepeatingPage(fcChoice),
      mkSectionNonRepeatingPage(fcTextWithIncludeIf)
    )

    val expected = mkSectionNonRepeatingPage(fcTextExpected)

    val formTemplate: FormTemplate = mkFormTemplate(sections)
    val obtainedF: Future[Either[UnexpectedState, FormTemplate]] = rewriter.rewrite(formTemplate).value

    obtainedF.map { obtained =>
      assertEquals(obtained.map(_.sections(1)), Right(expected))
    }
  }

  private val fcA = FormCtx(FormComponentId("a"))
  private val invalidIfElseTable: List[(IfElse, String)] = List(
    (
      IfElse(Equals(fcA, Constant("1")), Constant("foo"), Constant("bar")),
      "Operator '=' in combination with a choice component cannot be used in if-then-else expression. Use 'contains' operator instead. This is expression triggering this error: Equals(FormCtx(a),Constant(1))"
    ),
    (
      IfElse(
        Contains(fcA, Constant("0")),
        IfElse(Equals(fcA, Constant("1")), Constant("A"), Constant("B")),
        Constant("C")
      ),
      "Operator '=' in combination with a choice component cannot be used in if-then-else expression. Use 'contains' operator instead. This is expression triggering this error: Equals(FormCtx(a),Constant(1))"
    ),
    (
      IfElse(
        Contains(fcA, Constant("0")),
        IfElse(Contains(fcA, Constant("2")), Constant("A"), Constant("B")),
        Constant("C")
      ),
      "Expression 'a contains 2' has wrong index 2. Choice a has only 2 elements. Use index from 0 to 1"
    )
  )

  invalidIfElseTable.foreach { case (ifElse, expectedError) =>
    test("Detect if-then-else expression which has wrong Equals or Contains usage in " + ifElse) {

      val yesNoLocalisedStrings = NonEmptyList.of(toSmartString("Yes"), toSmartString("No"))

      val choice = Choice(YesNo, yesNoLocalisedStrings, Horizontal, Nil, None, None)

      val fcChoice = mkFormComponent("a", choice, false)
      val fcTextGenesis = mkFormComponent("b", Value)

      val label =
        SmartString(
          LocalisedString(Map(LangADT.En -> "{0}")),
          List(ifElse)
        )

      val fcText = fcTextGenesis.copy(label = label)

      val sections = List(
        mkSectionNonRepeatingPage(fcChoice),
        mkSectionNonRepeatingPage(fcText)
      )

      val formTemplate: FormTemplate = mkFormTemplate(sections)
      val obtainedF: Future[Either[UnexpectedState, FormTemplate]] = rewriter.rewrite(formTemplate).value

      obtainedF.map { obtained =>
        assertEquals(obtained.map(_.sections(1)), Left(UnexpectedState(expectedError)))
      }
    }
  }

}
