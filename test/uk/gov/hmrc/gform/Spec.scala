/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.concurrent.{ Eventually, ScalaFutures }
import org.scalatest.matchers.{ BeMatcher, MatchResult, Matcher }
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.ExampleData

import scala.concurrent.ExecutionContext

trait Spec
  extends FlatSpecLike
  with Matchers
  with DiagrammedAssertions
  with TryValues
  with EitherValues
  with OptionValues
  with AppendedClues
  with ScalaFutures
  with StreamlinedXml
  with JsResultMatcher
  with MockFactory
  with Inside
  with Eventually
  with ExampleData
  with PropertyChecks {

  implicit lazy val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
}

trait JsResultMatcher { self: Spec =>

  def beJsSuccess[E](element: E): Matcher[JsResult[E]] = new BeJsSuccess[E](element)

  def jsError[E]: BeMatcher[JsResult[E]] = new IsJsErrorMatcher[E]

  final private class BeJsSuccess[E](element: E) extends Matcher[JsResult[E]] {
    def apply(jsResult: JsResult[E]): MatchResult = {
      MatchResult(
        jsResult.fold(_ => false, _ == element),
        s"'$jsResult' did not contain an element matching '$element'.",
        s"'$jsResult' contained an element matching '$element', but should not have.")
    }
  }

  final private class IsJsErrorMatcher[E] extends BeMatcher[JsResult[E]] {
    def apply(jsResult: JsResult[E]): MatchResult = MatchResult(
      jsResult.isError,
      s"'$jsResult' was not an JsError, but should have been.",
      s"'$jsResult' was an JsError, but should *NOT* have been.")
  }
}
