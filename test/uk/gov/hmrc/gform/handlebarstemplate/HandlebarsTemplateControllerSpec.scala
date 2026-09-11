/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.handlebarstemplate

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{ verify, when }
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.http.{ HeaderNames, MimeTypes, Status }
import play.api.mvc.Headers
import play.api.test.{ DefaultAwaitTimeout, FakeRequest, FutureAwaits, Helpers, ResultExtractors, Writeables }
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, RequestHandlerAlg }
import uk.gov.hmrc.gform.sharedmodel.{ HandlebarsTemplate, HandlebarsTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, FormTemplateRawId }
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.Future

class HandlebarsTemplateControllerSpec
  extends AnyWordSpec with Matchers with OptionValues with HeaderNames with Status with MimeTypes
  with DefaultAwaitTimeout with ResultExtractors with Writeables with FutureAwaits with MockitoSugar {

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  "upsert" should {
    "save the handlebars template when the existing form template cannot be read" in {
      val handlebarsTemplateAlgebra = mock[HandlebarsTemplateAlgebra[Future]]
      val formTemplateService = mock[FormTemplateService]
      val requestHandler = mock[RequestHandlerAlg[FOpt]]
      val controller = new HandlebarsTemplateController(
        Helpers.stubControllerComponents(),
        handlebarsTemplateAlgebra,
        requestHandler,
        formTemplateService
      )
      val handlebarsTemplateId = HandlebarsTemplateId("upload-fail-test-testDestination")

      when(formTemplateService.get(FormTemplateRawId("upload-fail-test"))).thenReturn(
        Future.failed(new NoSuchElementException("raw template not found"))
      )
      when(formTemplateService.get(FormTemplateId("upload-fail-test"))).thenReturn(
        Future.failed(new IllegalArgumentException("outdated form template"))
      )
      when(handlebarsTemplateAlgebra.save(any[HandlebarsTemplate])).thenReturn(Future.successful(()))

      val result = controller.upsert(handlebarsTemplateId)(
        FakeRequest("POST", "/", Headers(), "{\"testValue\":\"{{testValue}}\"}")
      )

      status(result) shouldBe Status.OK
      contentAsString(result) should include("form template not available")
      verify(handlebarsTemplateAlgebra).save(any[HandlebarsTemplate])
    }
  }
}
