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

package uk.gov.hmrc.gform.submission.destinations

import cats.syntax.eq._
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, FormGen }
import uk.gov.hmrc.gform.{ Possible, Spec, possibleMonadError }
import uk.gov.hmrc.http.HeaderCarrier

class StateTransitionsServiceSpec extends Spec with DestinationGen with FormGen {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  "A Destination.StateTransition" should "return successfully if the state transition succeeds" in {
    forAll(
      stateTransitionGen(failOnError = Some(true)),
      formIdGen
    ) { (destination, formId) =>
      createService
        .expectUpdateFormStatus(formId, destination.requiredState, destination.requiredState)
        .sut
        .apply(destination, formId) shouldBe Right(())
    }
  }

  it should "return successfully if the state transition fails but failOnError is false" in {
    forAll(
      stateTransitionGen(failOnError = Some(false)),
      formIdGen,
      formStatusGen
    ) { (destination, formId, returnedFormStatus) =>
      whenever(returnedFormStatus =!= destination.requiredState) {
        createService
          .expectUpdateFormStatus(formId, destination.requiredState, returnedFormStatus)
          .sut
          .apply(destination, formId) shouldBe Right(())
      }
    }
  }

  it should "return unsuccessfully if the state transition fails and failOnError is true" in {
    forAll(
      stateTransitionGen(failOnError = Some(true)),
      formIdGen,
      formStatusGen
    ) { (destination, formId, returnedFormStatus) =>
      whenever(returnedFormStatus =!= destination.requiredState) {
        createService
          .expectUpdateFormStatus(formId, destination.requiredState, returnedFormStatus)
          .sut
          .apply(destination, formId) shouldBe raiseError(
          formId,
          destination.id,
          StateTransitionAlgebra.failedToAchieveStateTransition(destination, returnedFormStatus))
      }
    }
  }

  case class ServiceParts(sut: StateTransitionService[Possible], formAlgebra: FormAlgebra[Possible]) {
    def expectUpdateFormStatus(formId: FormId, requiredStatus: FormStatus, returnStatus: FormStatus): ServiceParts = {
      (formAlgebra
        .updateFormStatus(_: FormId, _: FormStatus)(_: HeaderCarrier))
        .expects(formId, requiredStatus, hc)
        .returning(Right(returnStatus))
      this
    }
  }

  private def createService: ServiceParts = {
    val formAlgebra = mock[FormAlgebra[Possible]]
    val submitter =
      new StateTransitionService[Possible](formAlgebra)

    ServiceParts(submitter, formAlgebra)
  }
}
