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

package uk.gov.hmrc.gform

import play.api.libs.json._
import play.api.mvc.{ BodyParser, BodyParsers }
import play.api.libs.iteratee.Execution.Implicits.trampoline
import uk.gov.hmrc.gform.exceptions.InvalidStateWithJson

package object controllers {

  object JsonExtractor {
    def apply[A, B](rdsA: Reads[A])(implicit rdsB: Reads[B]): BodyParser[(A, B)] = {
      BodyParsers.parse.json[JsObject].validate { json =>
        val resultAB = for {
          a <- rdsA.reads(json)
          b <- rdsB.reads(json)
        } yield (a, b)
        resultAB match {
          case JsSuccess((a, b), _) => Right((a, b))
          case JsError(error) => Left(InvalidStateWithJson("Cannot read data from json. Got:" + error, json).toResult)
        }
      }
    }
  }
}
