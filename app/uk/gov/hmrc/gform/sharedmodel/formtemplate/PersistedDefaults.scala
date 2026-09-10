/*
 * Copyright 2026 HM Revenue & Customs
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
import julienrf.json.derived.{ DerivedOWrites, TypeTag }
import play.api.libs.json.{ OFormat, Reads }
import shapeless.Lazy

/** julienrf.json.derived ignores Scala default arguments, so a field added to a persisted case class with a default is
  * mandatory on read and documents written before it existed stop decoding.
  *
  * Pairing a defaults-aware `Reads` (play-json's `WithDefaultValues` macro reads the defaults off the constructor) with
  * the derived `OWrites` keeps reads tolerant while leaving the persisted shape byte-identical.
  *
  * julienrf prefers an implicit instance for a member of a sealed hierarchy over deriving one structurally, so
  * declaring the result in the case class companion is enough.
  */
object PersistedDefaults {

  def oformat[A](
    reads: Reads[A]
  )(implicit derivedOWrites: Lazy[DerivedOWrites[A, TypeTag.ShortClassName]]): OFormat[A] =
    OFormat(reads, derived.owrites[A]())
}
