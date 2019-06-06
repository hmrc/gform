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

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }

object Helpers {
  def toLocalisedString(string: String) =
    LocalisedString(Map(LangADT.En -> string))

  def toLocalisedString(string: Option[String]): Option[LocalisedString] = string.map(s => toLocalisedString(s))

  def toLocalisedString(stringList: NonEmptyList[String]): NonEmptyList[LocalisedString] =
    stringList.map(s => toLocalisedString(s))

  def toLocalisedStrings(optionalStringList: Option[List[String]]): Option[List[LocalisedString]] =
    optionalStringList.map(stringList => stringList.map(string => toLocalisedString(string)))

}
