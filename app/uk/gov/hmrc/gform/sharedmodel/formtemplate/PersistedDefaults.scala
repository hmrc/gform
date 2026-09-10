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

import play.api.libs.json.{ JsObject, JsValue, Reads }

/** julienrf.json.derived ignores Scala default arguments, so a field added to a persisted case class with a default is
  * mandatory on read. Documents written before that field existed then fail to decode. These helpers re-supply the
  * default when the field is absent.
  */
object PersistedDefaults {

  type Defaults = Seq[(String, JsValue)]

  /** For ADTs persisted as `{"Tag": { ... }}`. */
  def tagged(defaultsByTag: Map[String, Defaults]): JsValue => JsValue = {
    case jsObject: JsObject =>
      defaultsByTag.foldLeft(jsObject) { case (acc, (tag, defaults)) =>
        (acc \ tag).asOpt[JsObject].fold(acc)(inner => acc + (tag -> withDefaults(inner, defaults)))
      }
    case other => other
  }

  /** For case classes persisted as a flat object. */
  def flat(defaults: Defaults): JsValue => JsValue = {
    case jsObject: JsObject => withDefaults(jsObject, defaults)
    case other              => other
  }

  def reads[A](fill: JsValue => JsValue)(underlying: Reads[A]): Reads[A] =
    Reads(json => underlying.reads(fill(json)))

  private def withDefaults(jsObject: JsObject, defaults: Defaults): JsObject =
    defaults.foldLeft(jsObject) { case (acc, (field, default)) =>
      if (acc.keys.contains(field)) acc else acc + (field -> default)
    }
}
