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

package uk.gov.hmrc.gform

import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }

object Helpers {
  def toLocalisedString(string: String) =
    LocalisedString(Map(LangADT.En -> string))

  def toLocalisedString(stringEn: String, stringCy: String) =
    LocalisedString(Map(LangADT.En -> stringEn, LangADT.Cy -> stringCy))

  def toSmartString(string: String): SmartString =
    SmartString(toLocalisedString(string), Nil)

  def toSmartString(stringEn: String, stringCy: String): SmartString =
    SmartString(LocalisedString(Map(LangADT.En -> stringEn, LangADT.Cy -> stringCy)), Nil)
}
