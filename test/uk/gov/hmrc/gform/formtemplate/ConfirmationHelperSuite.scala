/*
 * Copyright 2025 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthCtx, AuthInfo, Concat, Confirmation, Expr, FormComponentId, FormCtx, FormTemplate, PostcodeLookup, Value }

class ConfirmationHelperSuite extends FunSuite with FormTemplateSupport {
  test("Replace postcodeLookup field in confirmation - single postcodeLookup (1)") {
    val in = mkConfirmation(
      Some(NonEmptyList.of(FormComponentId("importerAddress"), FormComponentId("firstName"))),
      Some(NonEmptyList.one(AuthCtx(AuthInfo.PayeNino)))
    )

    val out = mkConfirmation(
      Some(NonEmptyList.one(FormComponentId("firstName"))),
      Some(NonEmptyList.of(FormCtx(FormComponentId("importerAddress")), AuthCtx(AuthInfo.PayeNino)))
    )

    testConfirmation(in, out)
  }

  test("Replace postcodeLookup field in confirmation - single postcodeLookup (2)") {
    val in = mkConfirmation(
      Some(NonEmptyList.of(FormComponentId("importerAddress"), FormComponentId("firstName"))),
      None
    )

    val out = mkConfirmation(
      Some(NonEmptyList.one(FormComponentId("firstName"))),
      Some(NonEmptyList.one(FormCtx(FormComponentId("importerAddress"))))
    )

    testConfirmation(in, out)
  }

  test("Replace postcodeLookup field in confirmation - single postcodeLookup (3)") {
    val in = mkConfirmation(
      Some(NonEmptyList.one(FormComponentId("importerAddress"))),
      None
    )

    val out = mkConfirmation(
      None,
      Some(NonEmptyList.one(FormCtx(FormComponentId("importerAddress"))))
    )

    testConfirmation(in, out)
  }

  test("Replace postcodeLookup field in confirmation - single postcodeLookup(4)") {
    val in = mkConfirmation(
      Some(NonEmptyList.of(FormComponentId("importerAddress"), FormComponentId("firstName"))),
      Some(NonEmptyList.one(FormCtx(FormComponentId("importerAddress"))))
    )

    val out = mkConfirmation(
      Some(NonEmptyList.one(FormComponentId("firstName"))),
      Some(NonEmptyList.one(FormCtx(FormComponentId("importerAddress"))))
    )

    testConfirmation(in, out)
  }

  test("Replace postcodeLookup field in confirmation - multiple postcodeLookups (1)") {
    val in = mkConfirmation(
      Some(
        NonEmptyList
          .of(FormComponentId("importerAddress"), FormComponentId("firstName"), FormComponentId("importerAddress2"))
      ),
      None
    )

    val out = mkConfirmation(
      Some(NonEmptyList.one(FormComponentId("firstName"))),
      Some(NonEmptyList.of(FormCtx(FormComponentId("importerAddress")), FormCtx(FormComponentId("importerAddress2"))))
    )

    testConfirmation(in, out)
  }

  test("Replace postcodeLookup field in confirmation - multiple postcodeLookups (2)") {
    val in = mkConfirmation(
      Some(
        NonEmptyList
          .of(FormComponentId("importerAddress"), FormComponentId("firstName"), FormComponentId("importerAddress2"))
      ),
      Some(
        NonEmptyList.of(FormCtx(FormComponentId("importerAddress")), FormCtx(FormComponentId("importerAddress2")))
      )
    )

    val out = mkConfirmation(
      Some(NonEmptyList.one(FormComponentId("firstName"))),
      Some(NonEmptyList.of(FormCtx(FormComponentId("importerAddress")), FormCtx(FormComponentId("importerAddress2"))))
    )

    testConfirmation(in, out)
  }

  test("Replace field refs in confirmedExpressions (1)") {
    val in = mkConfirmation(
      Some(NonEmptyList.one(FormComponentId("importerAddress"))),
      Some(NonEmptyList.of(Concat(List(FormCtx(FormComponentId("lastName")))), FormCtx(FormComponentId("firstName"))))
    )

    val out = mkConfirmation(
      Some(NonEmptyList.one(FormComponentId("firstName"))),
      Some(
        NonEmptyList
          .of(FormCtx(FormComponentId("importerAddress")), Concat(List(FormCtx(FormComponentId("lastName")))))
      )
    )

    testConfirmation(in, out)
  }

  test("Replace field refs in confirmedExpressions (2)") {
    val in = mkConfirmation(
      Some(NonEmptyList.one(FormComponentId("lastName"))),
      Some(NonEmptyList.one(FormCtx(FormComponentId("firstName"))))
    )

    val out = mkConfirmation(
      Some(NonEmptyList.of(FormComponentId("lastName"), FormComponentId("firstName"))),
      None
    )

    testConfirmation(in, out)
  }

  test("Replace field refs in confirmedExpressions (3)") {
    val in = mkConfirmation(
      Some(NonEmptyList.one(FormComponentId("lastName"))),
      Some(NonEmptyList.one(FormCtx(FormComponentId("lastName"))))
    )

    val out = mkConfirmation(
      Some(NonEmptyList.of(FormComponentId("lastName"))),
      None
    )

    testConfirmation(in, out)
  }

  test("Replace field refs in confirmedExpressions (4)") {
    val in = mkConfirmation(
      None,
      Some(NonEmptyList.one(FormCtx(FormComponentId("lastName"))))
    )

    val out = mkConfirmation(
      Some(NonEmptyList.of(FormComponentId("lastName"))),
      None
    )

    testConfirmation(in, out)
  }

  test("Keep confirm as is if nothing needs to be done") {
    val in = mkConfirmation(
      Some(NonEmptyList.one(FormComponentId("firstName"))),
      Some(NonEmptyList.one(Concat(List(FormCtx(FormComponentId("lastName"))))))
    )

    testConfirmation(in, in)
  }

  def testConfirmation(confirmation: Confirmation, expectedConfirmation: Confirmation) = {
    val formTemplate = mkFormTemplate(
      List(
        mkSectionNonRepeatingPage(
          formComponents = List(
            mkFormComponent(id = "firstName", Value),
            mkFormComponent(id = "lastName", Value),
            mkFormComponent(id = "importerAddress", PostcodeLookup(None, None, None), true),
            mkFormComponent(id = "importerAddress2", PostcodeLookup(None, None, None), true)
          )
        ),
        mkSectionNonRepeatingPage(
          formComponents = List(),
          confirmation = Some(confirmation)
        )
      )
    )
    val updatedFormTemplate = ConfirmationHelper(formTemplate).rewriteConfirmation()

    val confirmations = extractConfirmations(updatedFormTemplate)

    confirmations.foreach { confirmation =>
      val fieldsConfirmed = confirmation.fieldsConfirmed
      val expressionsConfirmed = confirmation.expressionsConfirmed

      assertEquals(fieldsConfirmed, expectedConfirmation.fieldsConfirmed)
      assertEquals(expressionsConfirmed, expectedConfirmation.expressionsConfirmed)
    }
  }

  def mkConfirmation(
    fieldsConfirmed: Option[NonEmptyList[FormComponentId]],
    expressionsConfirmed: Option[NonEmptyList[Expr]]
  ): Confirmation = Confirmation(
    question = addAnotherQuestion,
    redirects = None,
    fieldsConfirmed = fieldsConfirmed,
    expressionsConfirmed = expressionsConfirmed
  )

  def extractConfirmations(formTemplate: FormTemplate): List[Confirmation] =
    formTemplate.formKind.allSections.flatMap { section =>
      section.fold(nonRepeatingPage => nonRepeatingPage.page.confirmation.toList) { repeatingPage =>
        repeatingPage.page.confirmation.toList
      } { addToList =>
        addToList.pages.toList.flatMap(_.confirmation)
      }
    }

}
