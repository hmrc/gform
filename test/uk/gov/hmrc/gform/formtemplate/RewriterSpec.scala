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

package uk.gov.hmrc.gform.formtemplate

import cats.data.NonEmptyList
import munit.FunSuite
import scala.concurrent.Future
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class RewriterSpec extends FunSuite with FormTemplateSupport {

  val rewriter = new Rewriter {}

  test("Rewrite includeIf when Equals refers to Choice component") {

    val choice = Choice(
      YesNo,
      yesNoLocalisedStrings,
      Horizontal,
      Nil,
      None,
      None,
      None,
      LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
      None,
      None
    )

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
      assertEquals(obtained.map(_.formKind.allSections(1)), Right(expected))
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

      val choice = Choice(
        YesNo,
        yesNoLocalisedStrings,
        Horizontal,
        Nil,
        None,
        None,
        None,
        LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
        None,
        None
      )

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
        assertEquals(
          obtained.map(_.formKind.allSections(1)),
          Left(UnexpectedState(expectedError))
        )
      }
    }
  }

  test("Task captions are propagated through all its sections when its sections all don't have captions") {
    val pageTitle1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page title 1")), Nil)
    val page1 = defaultPage.copy(title = pageTitle1)

    val pageTitle2 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page title 2")), Nil)
    val page2 = defaultPage.copy(title = pageTitle2)

    val section1 = Section.NonRepeatingPage(page = page1)
    val section2 = Section.NonRepeatingPage(page = page2)

    val taskTitle1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task title 1")), Nil)
    val taskCaption1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task caption")), Nil)
    val task1 = Task(
      title = taskTitle1,
      sections = NonEmptyList(section1, List(section2)),
      caption = Some(taskCaption1),
      summarySection = None,
      declarationSection = None,
      includeIf = None
    )

    val taskSectionTitle1 =
      SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task section title 1")), Nil)
    val taskSection1 = TaskSection(
      title = taskSectionTitle1,
      tasks = NonEmptyList(task1, Nil)
    )

    val sections: NonEmptyList[TaskSection] = NonEmptyList(taskSection1, Nil)

    val expectedSections: List[Section] =
      List(page1.copy(caption = Some(taskCaption1)), page2.copy(caption = Some(taskCaption1)))
        .map(Section.NonRepeatingPage)

    val formTemplate = ExampleData.formTemplate.copy(formKind = FormKind.TaskList(sections), emailParameters = None)

    val obtainedF: Future[Either[UnexpectedState, FormTemplate]] = rewriter.rewrite(formTemplate).value

    obtainedF.map { obtained =>
      obtained.map(_.formKind.allSections.zipAll(expectedSections, None, None).foreach {
        case (section, expectedSection) => assertEquals(section, expectedSection)
      })
    }
  }

  test("Task captions are propagated through only its sections that do not have captions") {
    val pageTitle1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page title 1")), Nil)
    val pageCaption1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page caption 1")), Nil)
    val page1 = defaultPage.copy(title = pageTitle1, caption = Some(pageCaption1))

    val pageTitle2 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page title 2")), Nil)
    val page2 = defaultPage.copy(title = pageTitle2)

    val section1 = Section.NonRepeatingPage(page = page1)
    val section2 = Section.NonRepeatingPage(page = page2)

    val taskTitle1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task title 1")), Nil)
    val taskCaption1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task caption")), Nil)
    val task1 = Task(
      title = taskTitle1,
      sections = NonEmptyList(section1, List(section2)),
      caption = Some(taskCaption1),
      summarySection = None,
      declarationSection = None,
      includeIf = None
    )

    val taskSectionTitle1 =
      SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task section title 1")), Nil)
    val taskSection1 = TaskSection(
      title = taskSectionTitle1,
      tasks = NonEmptyList(task1, Nil)
    )

    val sections: NonEmptyList[TaskSection] = NonEmptyList(taskSection1, Nil)

    val expectedSections: List[Section] =
      List(page1, page2.copy(caption = Some(taskCaption1))).map(Section.NonRepeatingPage)

    val formTemplate = ExampleData.formTemplate.copy(formKind = FormKind.TaskList(sections), emailParameters = None)

    val obtainedF: Future[Either[UnexpectedState, FormTemplate]] = rewriter.rewrite(formTemplate).value

    obtainedF.map { obtained =>
      obtained.map(_.formKind.allSections.zipAll(expectedSections, None, None).foreach {
        case (section, expectedSection) => assertEquals(section, expectedSection)
      })
    }
  }

  test("Task captions are not propagated when all its sections have captions") {
    val pageTitle1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page title 1")), Nil)
    val pageCaption1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page caption 1")), Nil)
    val page1 = defaultPage.copy(title = pageTitle1, caption = Some(pageCaption1))

    val pageTitle2 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page title 2")), Nil)
    val pageCaption2 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page caption 2")), Nil)
    val page2 = defaultPage.copy(title = pageTitle2, caption = Some(pageCaption2))

    val section1 = Section.NonRepeatingPage(page = page1)
    val section2 = Section.NonRepeatingPage(page = page2)

    val taskTitle1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task title 1")), Nil)
    val taskCaption1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task caption")), Nil)
    val task1 = Task(
      title = taskTitle1,
      sections = NonEmptyList(section1, List(section2)),
      caption = Some(taskCaption1),
      summarySection = None,
      declarationSection = None,
      includeIf = None
    )

    val taskSectionTitle1 =
      SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task section title 1")), Nil)
    val taskSection1 = TaskSection(
      title = taskSectionTitle1,
      tasks = NonEmptyList(task1, Nil)
    )

    val sections: NonEmptyList[TaskSection] = NonEmptyList(taskSection1, Nil)

    val expectedSections: List[Section] = List(page1, page2).map(Section.NonRepeatingPage)

    val formTemplate = ExampleData.formTemplate.copy(formKind = FormKind.TaskList(sections), emailParameters = None)

    val obtainedF: Future[Either[UnexpectedState, FormTemplate]] = rewriter.rewrite(formTemplate).value

    obtainedF.map { obtained =>
      obtained.map(_.formKind.allSections.zipAll(expectedSections, None, None).foreach {
        case (section, expectedSection) => assertEquals(section, expectedSection)
      })
    }
  }

}
