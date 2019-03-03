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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import uk.gov.hmrc.gform.Spec

class HandlebarsJsonValidatorSpec extends Spec {
  "null" should "be accepted" in {
    ok("null")
  }

  "boolean literals" should "be accepted" in {
    ok("true")
    ok("false")
  }

  "numeric literals" should "be accepted" in {
    ok("1")
    ok("1.2")
    ok("-1.2e23")
    ok("-1.2E-1")
  }

  "string literals" should "be accepted" in {
    ok("""""""")
    ok(""""a"""")
    ok(""""abc"""")
    ok(""""a\"b c"""")
  }

  "objects" should "be accepted when empty" in {
    ok("{}")
  }

  it should "be accepted with one field" in {
    ok("""{ "a" : "b" }""")
  }

  it should "be accepted with multiple fields" in {
    ok("""{ "a" : "b", "c": "d" }""")
  }

  "arrays" should "be accepted when empty" in {
    ok("[]")
  }

  it should "be accepted with one element" in {
    ok("""["a"]""")
  }

  it should "be accepted with multiple elements" in {
    ok("""["a", "b"]""")
  }

  "handlebars top level elements" should "be accepted with no args" in {
    ok("{{foo}}")
  }

  it should "be accepted with one arg" in {
    ok("{{foo bar}}")
  }

  it should "be accepted with multiple args" in {
    ok("""{{foo "bar" 'baz'}}""")
  }

  it should "be accepted with sub-expressions" in {
    ok("""{{foo (bar baz) woo (widget)}}""")
  }

  "handlebars blocks" should "be accepted with no arguments" in {
    ok("""|{{#foo}}
          |"xyz"
          |{{/foo}}
         """.stripMargin)
  }

  it should "be accepted with arguments" in {
    ok("""|{{#foo bar baz}}
          |"xyz",
          |{{/foo}}
       """.stripMargin)
  }

  "complicated cases" should "be accepted" in {
    ok("""|{
          |  "abc" : "{{foo (bar baz)}}",
          |  "def" : [
          |    {{#each foo}}
          |      {
          |        "bar" : {{@index}}
          |      },
          |    {{/each}},
          |    {{#if foo}}
          |      {
          |        "bar" : {{@index}}
          |      }
          |    {{else}}
          |      {
          |        "bar" : {{@index}}
          |      }
          |    {{/if}}
          |  ]
          |}""".stripMargin)
  }

  "mismatches" should "not not be allowed" in {
    val table = Table(
      ("json", "expectedMessage"),
      ("""{ "abc": "def" ] }""", "'}' expected but ']' found"),
      ("""[ "abc", "def" } ]""", "']' expected but '}' found"),
      (""" "{{foo "}} """, "'\"' expected but end of source found"),
      ("""{{#each foo}}"abc"{{/wrong}}""", "Expected {{/each}}. Got {{/wrong}}.")
    )

    forAll(table) {
      case (json, expectedMessage) =>
        notOk(json, expectedMessage)
    }
  }

  private def notOk(json: String, expectedMessage: String) =
    HandlebarsJsonValidator(json) match {
      case Left(e) if e.replace('`', '\'').endsWith(expectedMessage) => succeed
      case Left(e) =>
        fail(s"""Parse failed as expected, but got the wrong error. Expected "$expectedMessage" but got "$e".""")
      case Right(_) => fail(expectedMessage)
    }

  private def ok(s: String) = HandlebarsJsonValidator(s) shouldBe Right(s)
}
