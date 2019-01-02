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

package uk.gov.hmrc.gform.typeclasses

import java.time.{ LocalDate, LocalDateTime }

trait Now[T] {
  def apply(): T

}

object Now {

  def apply[T](value: T): Now[T] = new Now[T] {
    override val apply: T = value
  }

  implicit object LocalDateNow extends Now[LocalDate] {
    def apply: LocalDate = LocalDate.now()
  }

  implicit object LocalDateTimeNow extends Now[LocalDateTime] {
    def apply: LocalDateTime = LocalDateTime.now()
  }
}
