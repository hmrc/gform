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

package uk.gov.hmrc.gform.sharedmodel
import org.scalatest.Assertion
import org.scalactic.source.Position
import play.api.libs.json._
import uk.gov.hmrc.gform.Spec

package object formtemplate extends Spec {
  def verifyRead[T: Reads](expected: T, json: String)(implicit position: Position): Assertion =
    parseWithMargin(json) should beJsSuccess(expected)

  def verifyReadFailure[T: Reads](expectedFailureMessage: String, json: String): Assertion =
    parseWithMargin(json) should beJsError(expectedFailureMessage)

  def verifyRoundTrip[T](t: T)(implicit reads: Reads[T], writes: Writes[T]): Assertion =
    reads.reads(writes.writes(t)) should beJsSuccess(t)

  def parseWithMargin[T](json: String)(implicit reads: Reads[T]): JsResult[T] =
    reads.reads(Json.parse(json.stripMargin))
}
