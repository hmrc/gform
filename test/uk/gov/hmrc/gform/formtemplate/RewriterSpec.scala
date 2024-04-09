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

class RewriterSpec extends FunSuite with FormTemplateSupport with RewriterSupport {

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

  val captionTable = List(
    (
      Some(SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task caption")), Nil)),
      None,
      None
    ),
    (
      Some(SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task caption")), Nil)),
      Some(SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page caption 1")), Nil)),
      None
    ),
    (
      Some(SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task caption")), Nil)),
      Some(SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page caption 1")), Nil)),
      Some(SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page caption 2")), Nil))
    )
  )

  val taskCaptionTestNamesTable = List(
    "Task captions are propagated through all its sections when none of them have captions",
    "Task captions are propagated through only its sections that do not have captions",
    "Task captions are not propagated when all its sections have captions"
  )

  def generateRewrittenFormTemplate(
    section1: Section,
    section2: Section,
    taskCaption: Option[SmartString]
  ): Future[Either[UnexpectedState, FormTemplate]] = {
    val taskTitle1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English task title 1")), Nil)
    val task1 = Task(
      title = taskTitle1,
      sections = NonEmptyList(section1, List(section2)),
      caption = taskCaption,
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

    val formTemplate = ExampleData.formTemplate.copy(formKind = FormKind.TaskList(sections), emailParameters = None)

    rewriter.rewrite(formTemplate).value
  }

  captionTable.zip(taskCaptionTestNamesTable).foreach { case ((taskCaption, pageCaption1, pageCaption2), testName) =>
    val pageTitle1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page title 1")), Nil)
    val page1 = simplePage.copy(title = pageTitle1, caption = pageCaption1)

    val pageTitle2 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page title 2")), Nil)
    val page2 = simplePage.copy(title = pageTitle2, caption = pageCaption2)

    val allTests = List(
      (mkTaskListNonRepeatingPages(page1, page2, taskCaption), "only NonRepeatingPages"),
      (mkTaskListRepeatingPages(page1, page2, taskCaption), "only RepeatingPages"),
      (mkTaskListAddToList(page1, page2), "not for AddToList"),
      (
        mkTaskListNonRepeatingPageAndRepeatingPage(page1, page2, taskCaption),
        "for a mix of NonRepeatingPages and RepeatingPages"
      ),
      (
        mkTaskListNonRepeatingPageAndAddToList(page1, page2, taskCaption),
        "for a NonRepeatingPage but not an AddToList"
      ),
      (mkTaskListRepeatingPageAndAddToList(page1, page2, taskCaption), "for a RepeatingPage but not an AddToList")
    )

    allTests.foreach { case ((section1, section2, expectedSections), testDescription) =>
      test(testName + " - " + testDescription) {
        val obtainedF: Future[Either[UnexpectedState, FormTemplate]] =
          generateRewrittenFormTemplate(section1, section2, taskCaption)

        obtainedF.map { obtained =>
          obtained.map(_.formKind.allSections.zipAll(expectedSections, None, None).foreach {
            case (section, expectedSection) => assertEquals(section, expectedSection)
          })
        }
      }
    }
  }

  val atlCaptionTestNamesTable = List(
    "Add to list captions are propagated through all its pages when none of them have captions, including the default page",
    "Add to list captions are propagated through only its pages that do not have captions, including the default page",
    "Add to list captions are not propagated when all its pages have captions, including the default page"
  )

  captionTable.zip(atlCaptionTestNamesTable).foreach { case ((atlCaption, pageCaption1, pageCaption2), testName) =>
    test(testName) {
      val pageTitle1 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page title 1")), Nil)
      val page1 = simplePage.copy(title = pageTitle1, caption = pageCaption1)

      val pageTitle2 = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "English page title 2")), Nil)
      val page2 = simplePage.copy(title = pageTitle2, caption = pageCaption2)

      val defaultPageTitle = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "Default page title")), Nil)
      val defaultPageCaption =
        SmartString(localised = LocalisedString(m = Map(LangADT.En -> "Default page caption")), Nil)
      val defaultPage = simplePage.copy(title = defaultPageTitle, caption = Some(defaultPageCaption))

      val section1 =
        mkDefaultAtl(NonEmptyList(page1, List(page2))).copy(caption = atlCaption, defaultPage = Some(defaultPage))

      val expectedSections: List[Page] =
        List(
          page1.copy(caption = if (pageCaption1.isEmpty) atlCaption else pageCaption1),
          page2.copy(caption = if (pageCaption2.isEmpty) atlCaption else pageCaption2)
        )
      val expectedAtl = List(
        section1.copy(pages = NonEmptyList.fromListUnsafe(expectedSections), defaultPage = Some(defaultPage))
      )

      val formTemplate =
        ExampleData.formTemplate.copy(formKind = FormKind.Classic(List(section1)), emailParameters = None)

      val obtainedF: Future[Either[UnexpectedState, FormTemplate]] = rewriter.rewrite(formTemplate).value

      obtainedF.map { obtained =>
        obtained.map(_.formKind.allSections.zipAll(expectedAtl, None, None).foreach { case (section, expectedSection) =>
          assertEquals(section, expectedSection)
        })
      }
    }
  }
}
