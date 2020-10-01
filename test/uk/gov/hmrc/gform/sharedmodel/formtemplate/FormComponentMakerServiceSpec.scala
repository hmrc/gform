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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.formtemplate.FormComponentMakerService._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth._
import cats.syntax.either._
import org.scalatest.prop.TableDrivenPropertyChecks

class FormComponentMakerServiceSpec extends Spec with TableDrivenPropertyChecks {

  private val textConstraint: TextConstraint = BasicText
  private val shortTextConstraint: TextConstraint = ShortText.default

  private val expr: Expr = Value
  private val xsDisplayWidth = XS
  private val defaultDisplayWidth = DEFAULT

  "createTextObject" should "return a valid text object" in {

    val table = Table(
      ("actual", "expected"),
      (
        createTextObject(Some(TextFormat(textConstraint)), Some(TextExpression(expr)), None, IsNotUpperCase),
        Text(textConstraint, expr).asRight),
      (
        createTextObject(Some(TextFormat(textConstraint)), Some(TextExpression(expr)), None, IsUpperCase),
        Text(textConstraint, expr, defaultDisplayWidth, IsUpperCase).asRight),
      (
        createTextObject(None, Some(TextExpression(expr)), None, IsNotUpperCase),
        Text(shortTextConstraint, expr).asRight),
      (
        createTextObject(None, Some(TextExpression(expr)), None, IsUpperCase),
        Text(shortTextConstraint, expr, defaultDisplayWidth, IsUpperCase).asRight),
      (
        createTextObject(Some(TextFormat(textConstraint)), Some(TextExpression(expr)), Some("xs"), IsNotUpperCase),
        Text(textConstraint, expr, xsDisplayWidth).asRight),
      (
        createTextObject(Some(TextFormat(textConstraint)), Some(TextExpression(expr)), Some("xs"), IsUpperCase),
        Text(textConstraint, expr, xsDisplayWidth, IsUpperCase).asRight),
      (
        createTextObject(None, Some(TextExpression(expr)), Some("xs"), IsNotUpperCase),
        Text(shortTextConstraint, expr, xsDisplayWidth).asRight),
      (
        createTextObject(None, Some(TextExpression(expr)), Some("xs"), IsUpperCase),
        Text(shortTextConstraint, expr, xsDisplayWidth, IsUpperCase).asRight)
    )
    table.forEvery({ case (expected, result) => expected shouldBe result })
  }

}
