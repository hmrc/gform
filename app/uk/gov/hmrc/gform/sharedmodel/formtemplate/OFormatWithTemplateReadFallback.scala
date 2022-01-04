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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import julienrf.json.derived.{ DerivedOWrites, DerivedReads }
import play.api.libs.functional.{ Alternative, Applicative }
import play.api.libs.functional.syntax._
import play.api.libs.json.{ JsError, JsSuccess, JsValue, OFormat, Reads }

import scala.collection.Seq

object OFormatWithTemplateReadFallback {

  implicit def alternative(implicit a: Applicative[Reads]): Alternative[Reads] = new Alternative[Reads] {
    val app: Applicative[Reads] = a
    def |[A, B >: A](readsA: Reads[A], readsB: Reads[B]): Reads[B] =
      (js: JsValue) =>
        readsA.reads(js) match {
          case successA @ JsSuccess(_, _) => successA
          case JsError(_) => // ignore first read error
            readsB.reads(js) match {
              case successB @ JsSuccess(_, _) => successB
              case JsError(errorB)            => JsError(errorB)
            }
        }

    def empty: Reads[Nothing] = _ => JsError(Seq())
  }

  def apply[A: DerivedReads: DerivedOWrites](templateReads: Reads[A]): OFormat[A] = {
    val basic: OFormat[A] = derived.oformat[A]()
    val reads = (basic: Reads[A]) | templateReads
    OFormat(reads, basic)
  }
}
