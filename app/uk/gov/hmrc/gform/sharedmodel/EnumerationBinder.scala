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

import play.api.mvc.PathBindable
import uk.gov.hmrc.gform.allowedlist.AllowedListName
import uk.gov.hmrc.gform.allowedlist.AllowedListName.AllowedListName

object EnumerationBinder {
  implicit val allowedListNamePathBindable: PathBindable[AllowedListName] = new PathBindable[AllowedListName] {
    override def bind(key: String, value: String): Either[String, AllowedListName] =
      Right(AllowedListName.withName(value))

    override def unbind(key: String, value: AllowedListName): String = value.toString
  }
}
