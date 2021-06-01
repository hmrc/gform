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

package uk.gov.hmrc.gform.formtemplate

import cats.data.NonEmptyList
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ FlatSpecLike, Matchers }
import uk.gov.hmrc.gform.email.{ EmailRenderRequest, HMRCEmailRendererConnector, NotFound, ParametersNotFound, Successful, Unexpected }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, EmailParameter }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EmailTemplateVerifierSpec
    extends FlatSpecLike with Matchers with MockFactory with FormTemplateSupport with ScalaFutures {

  trait TestFixture {
    val mockConnector = mock[HMRCEmailRendererConnector]
    val emailVerifier = new EmailTemplateVerifier(mockConnector)
    implicit val hc: HeaderCarrier = HeaderCarrier()
  }

  "verifyEmailTemplate" should "return () when email template id and parameters are valid" in new TestFixture {

    (mockConnector
      .renderTemplate(_: EmailRenderRequest)(_: HeaderCarrier))
      .expects(EmailRenderRequest("some-template-id", Map("param1" -> "param1")), hc)
      .returns(Future.successful(Successful))

    val result = emailVerifier
      .verifyEmailTemplate(
        mkFormTemplate(List.empty).copy(
          emailTemplateId = "some-template-id",
          emailParameters = Some(NonEmptyList.one(EmailParameter("param1", Constant("1"))))
        )
      )
      .value
      .futureValue

    result shouldBe Right(())
  }

  it should "NotFound when email template id is invalid" in new TestFixture {

    (mockConnector
      .renderTemplate(_: EmailRenderRequest)(_: HeaderCarrier))
      .expects(*, *)
      .returns(Future.successful(NotFound))

    val result = emailVerifier
      .verifyEmailTemplate(
        mkFormTemplate(List.empty).copy(
          emailTemplateId = "some-template-id",
          emailParameters = None
        )
      )
      .value
      .futureValue

    result shouldBe Left(UnexpectedState("Email template 'some-template-id' not found"))
  }

  it should "ParametersNotFound(reason) when email parameters are missing" in new TestFixture {

    (mockConnector
      .renderTemplate(_: EmailRenderRequest)(_: HeaderCarrier))
      .expects(*, *)
      .returns(Future.successful(ParametersNotFound("parameter 'param1' missing")))

    val result = emailVerifier
      .verifyEmailTemplate(
        mkFormTemplate(List.empty).copy(
          emailTemplateId = "some-template-id",
          emailParameters = None
        )
      )
      .value
      .futureValue

    result shouldBe Left(UnexpectedState("parameter 'param1' missing"))
  }

  it should "Unexpected(reason) when error is unknown" in new TestFixture {

    (mockConnector
      .renderTemplate(_: EmailRenderRequest)(_: HeaderCarrier))
      .expects(*, *)
      .returns(Future.successful(Unexpected("something went wrong")))

    val result = emailVerifier
      .verifyEmailTemplate(
        mkFormTemplate(List.empty).copy(
          emailTemplateId = "some-template-id",
          emailParameters = None
        )
      )
      .value
      .futureValue

    result shouldBe Left(UnexpectedState("something went wrong"))
  }
}
