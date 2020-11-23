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

import cats.syntax.either._
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.{ JsObject, JsString }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.FormComponentMakerService._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth._

class FormComponentMakerServiceSpec extends Spec with TableDrivenPropertyChecks {

  private val textConstraint: TextConstraint = BasicText

  private val expr: Expr = Value
  private val xsDisplayWidth = XS
  private val defaultDisplayWidth = DEFAULT

  "createTextObject" should "return a valid text object" in {

    val table = Table(
      ("actual", "expected"),
      (
        createTextObject(TextFormat(textConstraint), Some(TextExpression(expr)), None, IsNotUpperCase),
        Text(textConstraint, expr).asRight),
      (
        createTextObject(TextFormat(textConstraint), Some(TextExpression(expr)), None, IsUpperCase),
        Text(textConstraint, expr, defaultDisplayWidth, IsUpperCase).asRight),
      (
        createTextObject(TextFormat(textConstraint), Some(TextExpression(expr)), Some("xs"), IsNotUpperCase),
        Text(textConstraint, expr, xsDisplayWidth).asRight),
      (
        createTextObject(TextFormat(textConstraint), Some(TextExpression(expr)), Some("xs"), IsUpperCase),
        Text(textConstraint, expr, xsDisplayWidth, IsUpperCase).asRight)
    )
    table.forEvery({ case (expected, result) => expected shouldBe result })
  }

  "createObject" should "return error when format is empty for text type" in {
    val isMultiline = Some("no") // denotes text type
    val result = createObject(None, None, isMultiline, None, IsNotUpperCase, JsObject(Seq("id" -> JsString("text1"))))
    result shouldBe Left(UnexpectedState(s"""|Missing format for text field
                                             |Id: text1
                                             |Value: must supply a value
                                             |""".stripMargin))
  }

}
