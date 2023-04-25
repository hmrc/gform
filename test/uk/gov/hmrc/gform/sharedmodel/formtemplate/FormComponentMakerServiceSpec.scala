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
import play.api.libs.json.{ JsObject, JsString, Json }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.FormComponentMakerService._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.TextArea._
import uk.gov.hmrc.gform.Helpers.toSmartString

class FormComponentMakerServiceSpec extends Spec with TableDrivenPropertyChecks {

  private val textConstraint: TextConstraint = ShortText.default

  private val expr: Expr = Value
  private val xsDisplayWidth = XS
  private val defaultDisplayWidth = DEFAULT

  "createTextObject" should "return a valid text object" in {

    val table = Table(
      ("actual", "expected"),
      (
        createTextObject(
          TextFormat(textConstraint),
          Some(TextExpression(expr)),
          None,
          IsNotUpperCase,
          None,
          None,
          Json.obj()
        ),
        Text(textConstraint, expr).asRight
      ),
      (
        createTextObject(
          TextFormat(textConstraint),
          Some(TextExpression(expr)),
          None,
          IsUpperCase,
          None,
          None,
          Json.obj()
        ),
        Text(textConstraint, expr, defaultDisplayWidth, IsUpperCase).asRight
      ),
      (
        createTextObject(
          TextFormat(textConstraint),
          Some(TextExpression(expr)),
          Some("xs"),
          IsNotUpperCase,
          None,
          None,
          Json.obj()
        ),
        Text(textConstraint, expr, xsDisplayWidth).asRight
      ),
      (
        createTextObject(
          TextFormat(textConstraint),
          Some(TextExpression(expr)),
          Some("xs"),
          IsUpperCase,
          None,
          None,
          Json.obj()
        ),
        Text(textConstraint, expr, xsDisplayWidth, IsUpperCase).asRight
      ),
      (
        createTextObject(
          TextFormat(textConstraint),
          Some(TextExpression(expr)),
          Some("xs"),
          IsNotUpperCase,
          Some(toSmartString("prefixTest")),
          None,
          Json.obj()
        ),
        Text(textConstraint, expr, xsDisplayWidth, prefix = Some(toSmartString("prefixTest"))).asRight
      ),
      (
        createTextObject(
          TextFormat(textConstraint),
          Some(TextExpression(expr)),
          Some("xs"),
          IsNotUpperCase,
          Some(toSmartString("prefixTest")),
          Some(toSmartString("suffixTest")),
          Json.obj()
        ),
        Text(
          textConstraint,
          expr,
          xsDisplayWidth,
          prefix = Some(toSmartString("prefixTest")),
          suffix = Some(toSmartString("suffixTest"))
        ).asRight
      )
    )
    table.forEvery({ case (expected, result) => expected shouldBe result })
  }

  "createTextAreaObject" should "return a valid TextArea object" in {

    val table = Table(
      ("actual", "expected"),
      (
        createTextAreaObject(
          TextFormat(textConstraint),
          Some(TextExpression(expr)),
          None,
          Some("yes"),
          None,
          defaultRows,
          defaultDisplayCharCount,
          Json.obj()
        ),
        TextArea(textConstraint, expr, defaultDisplayWidth, dataThreshold = None).asRight
      ),
      (
        createTextAreaObject(
          TextFormat(textConstraint),
          Some(TextExpression(expr)),
          Some("xs"),
          Some("true"),
          Some(10),
          defaultRows,
          defaultDisplayCharCount,
          Json.obj()
        ),
        TextArea(textConstraint, expr, xsDisplayWidth, dataThreshold = Some(10)).asRight
      ),
      (
        createTextAreaObject(
          TextFormat(textConstraint),
          Some(TextExpression(expr)),
          Some("xs"),
          Some("true"),
          None,
          10,
          defaultDisplayCharCount,
          Json.obj()
        ),
        TextArea(textConstraint, expr, xsDisplayWidth, 10, dataThreshold = None).asRight
      ),
      (
        createTextAreaObject(
          TextFormat(textConstraint),
          Some(TextExpression(expr)),
          None,
          Some("yes"),
          None,
          defaultRows,
          false,
          Json.obj()
        ),
        TextArea(textConstraint, expr, defaultDisplayWidth, displayCharCount = false, dataThreshold = None).asRight
      ),
      (
        createTextAreaObject(
          TextFormat(textConstraint),
          Some(TextExpression(expr)),
          None,
          Some("yes"),
          None,
          defaultRows,
          true,
          Json.obj()
        ),
        TextArea(textConstraint, expr, defaultDisplayWidth, dataThreshold = None).asRight
      )
    )
    table.forEvery({ case (expected, result) => expected shouldBe result })
  }

  "createObject" should "return error when format is empty for text type" in {
    val isMultiline = Some("no") // denotes text type
    val result =
      createObject(
        None,
        None,
        isMultiline,
        None,
        None,
        IsNotUpperCase,
        None,
        None,
        defaultRows,
        defaultDisplayCharCount,
        JsObject(Seq("id" -> JsString("text1")))
      )
    result shouldBe Left(UnexpectedState(s"""|Missing or invalid format for text field
                                             |Id: text1
                                             |Format: None
                                             |Value: None
                                             |""".stripMargin))
  }

  it should "return error when format is empty for multiline text type" in {
    val isMultiline = Some("yes") // denotes multiline text type
    val result =
      createObject(
        None,
        None,
        isMultiline,
        None,
        None,
        IsNotUpperCase,
        None,
        None,
        defaultRows,
        defaultDisplayCharCount,
        JsObject(Seq("id" -> JsString("text1")))
      )
    result shouldBe Left(UnexpectedState(s"""|Missing or invalid format for multiline text field
                                             |Id: text1
                                             |Format: None
                                             |Value: None
                                             |""".stripMargin))
  }

  it should "return error when format is empty for multiline text type having no of rows" in {
    val isMultiline = Some("yes") // denotes multiline text type
    val result =
      createObject(
        None,
        None,
        isMultiline,
        None,
        None,
        IsNotUpperCase,
        None,
        None,
        10,
        defaultDisplayCharCount,
        JsObject(Seq("id" -> JsString("text1")))
      )
    result shouldBe Left(UnexpectedState(s"""|Missing or invalid format for multiline text field
                                             |Id: text1
                                             |Format: None
                                             |Value: None
                                             |""".stripMargin))
  }

  it should "return error when format is invalid for text type" in {
    val result = createObject(
      Some(OrientationFormat("xxx")),
      None,
      None,
      None,
      None,
      IsNotUpperCase,
      None,
      None,
      defaultRows,
      defaultDisplayCharCount,
      JsObject(Seq("id" -> JsString("text1")))
    )
    result shouldBe Left(UnexpectedState(s"""|Missing or invalid format for text field
                                             |Id: text1
                                             |Format: Some(OrientationFormat(xxx))
                                             |Value: None
                                             |""".stripMargin))
  }

  it should "return error when format is invalid for multiline text type" in {
    val result = createObject(
      Some(OrientationFormat("xxx")),
      None,
      Some("yes"),
      None,
      None,
      IsNotUpperCase,
      None,
      None,
      defaultRows,
      defaultDisplayCharCount,
      JsObject(Seq("id" -> JsString("text1")))
    )
    result shouldBe Left(UnexpectedState(s"""|Missing or invalid format for multiline text field
                                             |Id: text1
                                             |Format: Some(OrientationFormat(xxx))
                                             |Value: None
                                             |""".stripMargin))
  }

  it should "return error when format is invalid for multiline text type having no of rows" in {
    val result = createObject(
      Some(OrientationFormat("xxx")),
      None,
      Some("yes"),
      None,
      None,
      IsNotUpperCase,
      None,
      None,
      10,
      defaultDisplayCharCount,
      JsObject(Seq("id" -> JsString("text1")))
    )
    result shouldBe Left(UnexpectedState(s"""|Missing or invalid format for multiline text field
                                             |Id: text1
                                             |Format: Some(OrientationFormat(xxx))
                                             |Value: None
                                             |""".stripMargin))
  }

}
