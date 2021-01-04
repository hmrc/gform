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

package uk.gov.hmrc.gform.submission.destinations

import cats.MonadError
import cats.syntax.eq._
import cats.syntax.flatMap._
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination
import uk.gov.hmrc.http.HeaderCarrier

trait StateTransitionAlgebra[M[_]] {
  def apply(d: Destination.StateTransition, formId: FormId)(implicit hc: HeaderCarrier): M[Unit]
}

object StateTransitionAlgebra {
  def failedToAchieveStateTransition(d: Destination.StateTransition, currentState: FormStatus): String =
    s"Cannot achieve transition from $currentState to ${d.requiredState}"
}

class StateTransitionService[M[_]](formAlgebra: FormAlgebra[M])(implicit monadError: MonadError[M, String])
    extends StateTransitionAlgebra[M] {
  override def apply(d: Destination.StateTransition, formId: FormId)(implicit hc: HeaderCarrier): M[Unit] =
    formAlgebra
      .updateFormStatus(formId, d.requiredState)
      .flatMap { stateAchieved =>
        if (stateAchieved === d.requiredState || !d.failOnError) monadError.pure(())
        else raiseError(formId, d.id, StateTransitionAlgebra.failedToAchieveStateTransition(d, stateAchieved))
      }
}
