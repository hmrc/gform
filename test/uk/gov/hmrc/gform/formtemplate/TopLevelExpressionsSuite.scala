/*
 * Copyright 2022 HM Revenue & Customs
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

import munit.FunSuite
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class TopLevelExpressionsSuite extends FunSuite {

  test("TopLevelExpressions (1)") {
    expressionsContextFromJson(
      Json.obj(
        "foo" -> "bar",
        "bar" -> "baz",
        "baz" -> "1 + 2"
      )
    ) { exprSubstitutions =>
      val graph = TopLevelExpressions.toGraph(exprSubstitutions)
      val res: Either[graph.NodeT, Iterable[(Int, List[ExpressionId])]] =
        TopLevelExpressions.toTopologicalSort(graph)

      res match {
        case Left(node) => fail("Unexpected cycle in graph" + node)
        case Right(iterable) =>
          assertEquals(
            iterable.toList,
            List(
              (0, List(ExpressionId("foo"))),
              (1, List(ExpressionId("bar"))),
              (2, List(ExpressionId("baz")))
            )
          )
      }
    }
  }

  test("TopLevelExpressions (2)") {
    check(
      Json.obj(
        "foo" -> "bar",
        "bar" -> "baz",
        "baz" -> "1 + 2"
      ),
      Map(
        ExpressionId("foo") -> Add(Constant("1"), Constant("2")),
        ExpressionId("bar") -> Add(Constant("1"), Constant("2")),
        ExpressionId("baz") -> Add(Constant("1"), Constant("2"))
      )
    )
  }

  test("TopLevelExpressions (3)") {
    check(
      Json.obj(
        "foo" -> "hello",
        "bar" -> "baz",
        "baz" -> "1 + 2"
      ),
      Map(
        ExpressionId("foo") -> FormCtx(FormComponentId("hello")),
        ExpressionId("bar") -> Add(Constant("1"), Constant("2")),
        ExpressionId("baz") -> Add(Constant("1"), Constant("2"))
      )
    )
  }

  test("TopLevelExpressions (4)") {
    check(
      Json.obj(
        "foo" -> "bar + baz",
        "bar" -> "5",
        "baz" -> "6"
      ),
      Map(
        ExpressionId("foo") -> Add(Constant("5"), Constant("6")),
        ExpressionId("bar") -> Constant("5"),
        ExpressionId("baz") -> Constant("6")
      )
    )
  }

  test("TopLevelExpressions (5)") {
    check(
      Json.obj(
        "foo" -> "bar + bar + baz + baz",
        "bar" -> "5",
        "baz" -> "6"
      ),
      Map(
        ExpressionId("foo") -> Add(Add(Add(Constant("5"), Constant("5")), Constant("6")), Constant("6")),
        ExpressionId("bar") -> Constant("5"),
        ExpressionId("baz") -> Constant("6")
      )
    )
  }

  test("TopLevelExpressions (6)") {
    check(
      Json.obj(
        "foo" -> "if a > 10 then bar else baz",
        "bar" -> "5",
        "baz" -> "6"
      ),
      Map(
        ExpressionId("foo") -> IfElse(
          GreaterThan(FormCtx(FormComponentId("a")), Constant("10")),
          Constant("5"),
          Constant("6")
        ),
        ExpressionId("bar") -> Constant("5"),
        ExpressionId("baz") -> Constant("6")
      )
    )
  }

  test("TopLevelExpressions (7)") {
    check(
      Json.obj(
        "choice1Content"     -> "'What is your name?'",
        "choice2Content"     -> "'What is your client’s name?'",
        "choice3Content"     -> "'What is the organisation’s name?'",
        "dynamicDefaultPage" -> "if choiceQuestion contains 0 then choice1Content else if choiceQuestion contains 1 then choice2Content else choice3Content"
      ),
      Map(
        ExpressionId("dynamicDefaultPage") -> IfElse(
          Contains(FormCtx(FormComponentId("choiceQuestion")), Constant("0")),
          Constant("What is your name?"),
          IfElse(
            Contains(FormCtx(FormComponentId("choiceQuestion")), Constant("1")),
            Constant("What is your client’s name?"),
            Constant("What is the organisation’s name?")
          )
        ),
        ExpressionId("choice1Content") -> Constant("What is your name?"),
        ExpressionId("choice2Content") -> Constant("What is your client’s name?"),
        ExpressionId("choice3Content") -> Constant("What is the organisation’s name?")
      )
    )
  }

  test("TopLevelExpressions (8)") {
    check(
      Json.obj(
        "foo" -> "if bar > 10 then bar else baz",
        "bar" -> "5",
        "baz" -> "6"
      ),
      Map(
        ExpressionId("foo") -> IfElse(
          GreaterThan(Constant("5"), Constant("10")),
          Constant("5"),
          Constant("6")
        ),
        ExpressionId("bar") -> Constant("5"),
        ExpressionId("baz") -> Constant("6")
      )
    )
  }

  test("TopLevelExpressions (9)") {
    check(
      Json.obj(
        "foo" -> "if bar + baz > x then bar else baz",
        "bar" -> "5",
        "baz" -> "6"
      ),
      Map(
        ExpressionId("foo") -> IfElse(
          GreaterThan(Add(Constant("5"), Constant("6")), FormCtx(FormComponentId("x"))),
          Constant("5"),
          Constant("6")
        ),
        ExpressionId("bar") -> Constant("5"),
        ExpressionId("baz") -> Constant("6")
      )
    )
  }

  test("TopLevelExpressions (10)") {
    check(
      Json.obj(
        "foo"    -> "1 + 4",
        "bar"    -> "baz",
        "baz"    -> "foo",
        "foobar" -> "foo + bar + baz"
      ),
      Map(
        ExpressionId("foo") -> Add(Constant("1"), Constant("4")),
        ExpressionId("bar") -> Add(Constant("1"), Constant("4")),
        ExpressionId("baz") -> Add(Constant("1"), Constant("4")),
        ExpressionId("foobar") -> Add(
          Add(
            Add(Constant("1"), Constant("4")),
            Add(Constant("1"), Constant("4"))
          ),
          Add(Constant("1"), Constant("4"))
        )
      )
    )
  }

  test("TopLevelExpressions (11)") {
    expressionsContextFromJson(
      Json.obj(
        "foo" -> "bar",
        "bar" -> "baz",
        "baz" -> "foo"
      )
    ) { exprSubstitutions =>
      TopLevelExpressions.resolveReferences(exprSubstitutions) match {
        case Left(node) =>
          assertEquals(
            node,
            UnexpectedState(
              "Cycle detected in top level expressions. Violating node is 'bar'. Graph contains cycle: Some(Cycle(ExpressionId(bar), ExpressionId(bar)~>ExpressionId(baz), ExpressionId(baz), ExpressionId(baz)~>ExpressionId(foo), ExpressionId(foo), ExpressionId(foo)~>ExpressionId(bar), ExpressionId(bar)))"
            )
          )
        case Right(iterable) => fail("Failed cycle detection")
      }
    }
  }

  private def check(json: JsValue, expressions: Map[ExpressionId, Expr]) =
    expressionsContextFromJson(json) { exprSubstitutions =>
      val exprSubstitutionsE = TopLevelExpressions.resolveReferences(exprSubstitutions)
      val expected = ExprSubstitutions(expressions)

      exprSubstitutionsE match {
        case Left(node) => fail("Unexpected cyclic dependency failure")
        case Right(res) => assertEquals(res, expected)
      }
    }

  private def expressionsContextFromJson[A](json: JsValue)(f: ExprSubstitutions => A): A = {
    val templateRaw = FormTemplateRaw(Json.obj("expressions" -> json))
    val expressionsContextOpt: Opt[ExprSubstitutions] = ExprSubstitutions.from(templateRaw)

    expressionsContextOpt match {
      case Left(UnexpectedState(error)) => fail("Invalid top level expressions " + error)
      case Right(exprSubstitutions)     => f(exprSubstitutions)
    }
  }
}