/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms

import play.api.libs.json._
import play.api.mvc.{ BodyParser, BodyParsers }
import play.api.libs.iteratee.Execution.Implicits.trampoline
import uk.gov.hmrc.bforms.exceptions.InvalidStateWithJson

package object controllers {

  object JsonWithKey {
    def apply[A, B](fieldName: String, f: String => B)(implicit rds: Reads[A]): BodyParser[(B, A)] = {
      BodyParsers.parse.json[JsObject].validate { json =>
        (json \ fieldName) match {
          case JsDefined(JsString(id)) =>
            rds.reads(json) match {
              case JsSuccess(a, _) => Right((f(id), a))
              case JsError(error) => Left(InvalidStateWithJson("Cannot parse json", json).toResult)
            }
          case otherwise => Left(InvalidStateWithJson(s"No fieldName '$fieldName' provided in json", json).toResult)
        }
      }
    }
  }
}
