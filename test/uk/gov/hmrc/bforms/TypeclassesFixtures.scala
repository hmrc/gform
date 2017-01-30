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

import play.api.libs.json.JsObject
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.bforms.core.Opt
import uk.gov.hmrc.bforms.model.{ DbOperationResult, FormTemplate, UpdateSuccess }
import uk.gov.hmrc.bforms.typeclasses.{ FindOne, Update }
import uk.gov.hmrc.play.http.HeaderCarrier

trait TypeclassCallCheck { def call(): Unit }
trait FindOneCheck extends TypeclassCallCheck
trait UpdateCheck extends TypeclassCallCheck

class NotUsedTypeclassError extends Error

trait TypeclassFixtures {

  class FindOneTC[B](callCheck: Option[FindOneCheck], returnValue: Option[B]) {
    def callCheck(callCheck: FindOneCheck) = new FindOneTC(Some(callCheck), returnValue)

    def noChecks = getTC(None)
    def withChecks(f: JsObject => Unit) = getTC(Some(f))

    private def getTC(checkFn: Option[JsObject => Unit]): FindOne[B] = new FindOne[B] {
      def apply(selector: JsObject): Future[Option[B]] = {
        callCheck.foreach(_.call())
        checkFn.foreach(_(selector))
        Future.successful(returnValue)
      }
    }
  }

  object FindOneTC {
    def response[A](returnValue: Option[A]) = new FindOneTC(None, returnValue)
  }

  class UpdateTC[B](callCheck: Option[UpdateCheck], returnValue: Opt[DbOperationResult]) {
    def callCheck(callCheck: UpdateCheck) = new UpdateTC[B](Some(callCheck), returnValue)

    def noChecks = getTC(None)
    def withChecks(f: (JsObject, B) => Unit) = getTC(Some(f))

    private def getTC(checkFn: Option[(JsObject, B) => Unit]): Update[B] = new Update[B] {
      def apply(selector: JsObject, v: B): Future[Opt[DbOperationResult]] = {
        callCheck.foreach(_.call())
        checkFn.foreach(_(selector, v))
        Future.successful(returnValue)
      }
    }
  }

  object UpdateTC {
    def response[A](returnValue: Opt[DbOperationResult]) = new UpdateTC[A](None, returnValue)
    def notUsed[B]: Update[B] = new Update[B] {
      def apply(selector: JsObject, v: B): Future[Opt[DbOperationResult]] = {
        Future.failed(new NotUsedTypeclassError)
      }
    }
  }
}
