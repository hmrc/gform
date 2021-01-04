/*
 * Copyright 2021 HM Revenue & Customs
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

import play.api.libs.json._

case class SectionNumber(value: Int) extends Ordered[SectionNumber] {
  override def compare(that: SectionNumber): Int = value.compare(that.value)
}

object SectionNumber {
  implicit val format: Format[SectionNumber] = Format[SectionNumber](
    Reads[SectionNumber] {
      case JsNumber(n: BigDecimal) => JsSuccess(SectionNumber(n.toInt))
      case unknown                 => JsError(s"JsNumber value expected, got: $unknown")
    },
    Writes[SectionNumber](a => JsNumber(a.value))
  )

  val firstSection = SectionNumber(0)
}
