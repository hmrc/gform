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

package uk.gov.hmrc.gform.formtemplate

import cats.data.EitherT
import cats.implicits._
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{ never, times, verify, when }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import play.api.libs.json.{ JsObject, JsString }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formredirect.FormRedirect
import uk.gov.hmrc.gform.gformfrontend.GformFrontendConnector
import uk.gov.hmrc.gform.handlebarstemplate.{ HandlebarsSchemaAlgebra, HandlebarsTemplateAlgebra }
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId, FormTemplateRaw }

import scala.concurrent.{ ExecutionContext, Future }

class FormTemplateServiceSpec extends AnyFlatSpec with Matchers with MockitoSugar with ScalaFutures with ExampleData {

  implicit private val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  "FormTemplateService.createE2e" should "successfully create an e2e form template from an existing form template" in {
    val formTemplateId = FormTemplateId("my-form")

    val rawJsObject = JsObject(
      Seq(
        "_id"      -> JsString("my-form"),
        "formName" -> JsString("My Form"),
        "version"  -> JsString("1")
      )
    )

    val formTemplateRawRepo = mock[Repo[FormTemplateRaw]]
    val formTemplateRepo = mock[Repo[FormTemplate]]
    val formRedirectRepo = mock[Repo[FormRedirect]]
    val handlebarsTemplateAlgebra = mock[HandlebarsTemplateAlgebra[FOpt]]
    val handlebarsSchemaAlgebra = mock[HandlebarsSchemaAlgebra[FOpt]]
    val configModule = mock[ConfigModule]
    val gformFrontendConnector = mock[GformFrontendConnector]

    val service = new FormTemplateService(
      formTemplateRepo,
      formTemplateRawRepo,
      formRedirectRepo,
      handlebarsTemplateAlgebra,
      handlebarsSchemaAlgebra,
      gformFrontendConnector,
      configModule
    )

    when(formTemplateRawRepo.getDocumentAsJson(formTemplateId.value))
      .thenReturn(Future.successful(rawJsObject))

    when(formTemplateRepo.find(formTemplateId.value))
      .thenReturn(Future.successful(Some(formTemplate)))

    when(formTemplateRawRepo.replace(any[FormTemplateRaw]))
      .thenReturn(EitherT.right[UnexpectedState](Future.successful(())))

    when(formTemplateRepo.replace(any[FormTemplate]))
      .thenReturn(EitherT.right[UnexpectedState](Future.successful(())))

    val result = service.createE2e(formTemplateId).futureValue

    result shouldBe ()
    verify(formTemplateRawRepo, times(1)).replace(any[FormTemplateRaw])
    verify(formTemplateRepo, times(1)).replace(any[FormTemplate])
  }

  it should "throw IllegalArgumentException when attempting to create e2e template with ID already ending in -e2e" in {
    val e2eFormTemplateId = FormTemplateId("my-form-e2e")

    val formTemplateRawRepo = mock[Repo[FormTemplateRaw]]
    val formTemplateRepo = mock[Repo[FormTemplate]]
    val formRedirectRepo = mock[Repo[FormRedirect]]
    val handlebarsTemplateAlgebra = mock[HandlebarsTemplateAlgebra[FOpt]]
    val handlebarsSchemaAlgebra = mock[HandlebarsSchemaAlgebra[FOpt]]
    val configModule = mock[ConfigModule]
    val gformFrontendConnector = mock[GformFrontendConnector]

    val service = new FormTemplateService(
      formTemplateRepo,
      formTemplateRawRepo,
      formRedirectRepo,
      handlebarsTemplateAlgebra,
      handlebarsSchemaAlgebra,
      gformFrontendConnector,
      configModule
    )

    val result = service.createE2e(e2eFormTemplateId).failed.futureValue

    result shouldBe an[IllegalArgumentException]
    result.getMessage should include("already an e2e form template")
    verify(formTemplateRawRepo, never).getDocumentAsJson(any[String])
  }

  it should "throw NoSuchElementException when original template does not exist" in {
    val formTemplateId = FormTemplateId("non-existent-form")

    val formTemplateRawRepo = mock[Repo[FormTemplateRaw]]
    val formTemplateRepo = mock[Repo[FormTemplate]]
    val formRedirectRepo = mock[Repo[FormRedirect]]
    val handlebarsTemplateAlgebra = mock[HandlebarsTemplateAlgebra[FOpt]]
    val handlebarsSchemaAlgebra = mock[HandlebarsSchemaAlgebra[FOpt]]
    val configModule = mock[ConfigModule]
    val gformFrontendConnector = mock[GformFrontendConnector]

    val service = new FormTemplateService(
      formTemplateRepo,
      formTemplateRawRepo,
      formRedirectRepo,
      handlebarsTemplateAlgebra,
      handlebarsSchemaAlgebra,
      gformFrontendConnector,
      configModule
    )

    val rawJsObject = JsObject(Seq("_id" -> JsString("non-existent-form")))

    when(formTemplateRawRepo.getDocumentAsJson(formTemplateId.value))
      .thenReturn(Future.successful(rawJsObject))

    when(formTemplateRepo.find(formTemplateId.value))
      .thenReturn(Future.successful(None))

    val result = service.createE2e(formTemplateId).failed.futureValue

    result shouldBe an[NoSuchElementException]
  }

  it should "throw Exception when repository replace fails" in {
    val formTemplateId = FormTemplateId("my-form")

    val rawJsObject = JsObject(
      Seq(
        "_id"      -> JsString("my-form"),
        "formName" -> JsString("My Form")
      )
    )

    val formTemplateRawRepo = mock[Repo[FormTemplateRaw]]
    val formTemplateRepo = mock[Repo[FormTemplate]]
    val formRedirectRepo = mock[Repo[FormRedirect]]
    val handlebarsTemplateAlgebra = mock[HandlebarsTemplateAlgebra[FOpt]]
    val handlebarsSchemaAlgebra = mock[HandlebarsSchemaAlgebra[FOpt]]
    val configModule = mock[ConfigModule]
    val gformFrontendConnector = mock[GformFrontendConnector]

    val service = new FormTemplateService(
      formTemplateRepo,
      formTemplateRawRepo,
      formRedirectRepo,
      handlebarsTemplateAlgebra,
      handlebarsSchemaAlgebra,
      gformFrontendConnector,
      configModule
    )

    when(formTemplateRawRepo.getDocumentAsJson(formTemplateId.value))
      .thenReturn(Future.successful(rawJsObject))

    when(formTemplateRepo.find(formTemplateId.value))
      .thenReturn(Future.successful(Some(formTemplate)))

    when(formTemplateRawRepo.replace(any[FormTemplateRaw]))
      .thenReturn(EitherT.left[Unit](Future.successful(UnexpectedState("Database error"))))

    val result = service.createE2e(formTemplateId).failed.futureValue

    result shouldBe an[Exception]
  }

  it should "verify JSON _id field is correctly updated with -e2e suffix" in {
    val formTemplateId = FormTemplateId("my-form")

    val rawJsObject = JsObject(
      Seq(
        "_id"      -> JsString("my-form"),
        "formName" -> JsString("My Form"),
        "version"  -> JsString("1")
      )
    )

    val formTemplateRawRepo = mock[Repo[FormTemplateRaw]]
    val formTemplateRepo = mock[Repo[FormTemplate]]
    val formRedirectRepo = mock[Repo[FormRedirect]]
    val handlebarsTemplateAlgebra = mock[HandlebarsTemplateAlgebra[FOpt]]
    val handlebarsSchemaAlgebra = mock[HandlebarsSchemaAlgebra[FOpt]]
    val configModule = mock[ConfigModule]
    val gformFrontendConnector = mock[GformFrontendConnector]

    val service = new FormTemplateService(
      formTemplateRepo,
      formTemplateRawRepo,
      formRedirectRepo,
      handlebarsTemplateAlgebra,
      handlebarsSchemaAlgebra,
      gformFrontendConnector,
      configModule
    )

    var capturedRawTemplate: Option[FormTemplateRaw] = None

    when(formTemplateRawRepo.getDocumentAsJson(formTemplateId.value))
      .thenReturn(Future.successful(rawJsObject))

    when(formTemplateRepo.find(formTemplateId.value))
      .thenReturn(Future.successful(Some(formTemplate)))

    when(formTemplateRawRepo.replace(any[FormTemplateRaw]))
      .thenAnswer { invocation =>
        val arg = invocation.getArgument[FormTemplateRaw](0)
        capturedRawTemplate = Some(arg)
        EitherT.right[UnexpectedState](Future.successful(()))
      }

    when(formTemplateRepo.replace(any[FormTemplate]))
      .thenReturn(EitherT.right[UnexpectedState](Future.successful(())))

    val result = service.createE2e(formTemplateId).futureValue

    result shouldBe ()
    capturedRawTemplate should not be empty
    val capturedJson = capturedRawTemplate.get.value
    (capturedJson \ "_id").as[String] should equal("my-form-e2e")
  }

  "FormTemplateService.get" should "retrieve a form template by ID" in {
    val formTemplateId = FormTemplateId("my-form")

    val formTemplateRawRepo = mock[Repo[FormTemplateRaw]]
    val formTemplateRepo = mock[Repo[FormTemplate]]
    val formRedirectRepo = mock[Repo[FormRedirect]]
    val handlebarsTemplateAlgebra = mock[HandlebarsTemplateAlgebra[FOpt]]
    val handlebarsSchemaAlgebra = mock[HandlebarsSchemaAlgebra[FOpt]]
    val configModule = mock[ConfigModule]
    val gformFrontendConnector = mock[GformFrontendConnector]

    val service = new FormTemplateService(
      formTemplateRepo,
      formTemplateRawRepo,
      formRedirectRepo,
      handlebarsTemplateAlgebra,
      handlebarsSchemaAlgebra,
      gformFrontendConnector,
      configModule
    )

    when(formTemplateRepo.get(formTemplateId.value))
      .thenReturn(Future.successful(formTemplate))

    val result = service.get(formTemplateId).futureValue

    result shouldBe formTemplate
  }

  "FormTemplateService.find" should "find a form template by ID" in {
    val formTemplateId = FormTemplateId("my-form")

    val formTemplateRawRepo = mock[Repo[FormTemplateRaw]]
    val formTemplateRepo = mock[Repo[FormTemplate]]
    val formRedirectRepo = mock[Repo[FormRedirect]]
    val handlebarsTemplateAlgebra = mock[HandlebarsTemplateAlgebra[FOpt]]
    val handlebarsSchemaAlgebra = mock[HandlebarsSchemaAlgebra[FOpt]]
    val configModule = mock[ConfigModule]
    val gformFrontendConnector = mock[GformFrontendConnector]

    val service = new FormTemplateService(
      formTemplateRepo,
      formTemplateRawRepo,
      formRedirectRepo,
      handlebarsTemplateAlgebra,
      handlebarsSchemaAlgebra,
      gformFrontendConnector,
      configModule
    )

    when(formTemplateRepo.find(formTemplateId.value))
      .thenReturn(Future.successful(Some(formTemplate)))

    val result = service.find(formTemplateId).futureValue

    result shouldBe Some(formTemplate)
  }

  "FormTemplateService.getAll" should "retrieve all raw form templates" in {
    val formTemplateRawRepo = mock[Repo[FormTemplateRaw]]
    val formTemplateRepo = mock[Repo[FormTemplate]]
    val formRedirectRepo = mock[Repo[FormRedirect]]
    val handlebarsTemplateAlgebra = mock[HandlebarsTemplateAlgebra[FOpt]]
    val handlebarsSchemaAlgebra = mock[HandlebarsSchemaAlgebra[FOpt]]
    val configModule = mock[ConfigModule]
    val gformFrontendConnector = mock[GformFrontendConnector]

    val service = new FormTemplateService(
      formTemplateRepo,
      formTemplateRawRepo,
      formRedirectRepo,
      handlebarsTemplateAlgebra,
      handlebarsSchemaAlgebra,
      gformFrontendConnector,
      configModule
    )

    val rawJs1 = JsObject(Seq("_id" -> JsString("form-1")))
    val rawJs2 = JsObject(Seq("_id" -> JsString("form-2")))
    val templates = List(FormTemplateRaw(rawJs1), FormTemplateRaw(rawJs2))

    when(formTemplateRawRepo.findAll())
      .thenReturn(Future.successful(templates))

    val result = service.getAll().futureValue

    result shouldBe templates
  }

  "FormTemplateService.save" should "save a form template raw" in {
    val rawJsObject = JsObject(
      Seq(
        "_id"      -> JsString("my-form"),
        "formName" -> JsString("My Form")
      )
    )

    val formTemplateRaw = FormTemplateRaw(rawJsObject)

    val formTemplateRawRepo = mock[Repo[FormTemplateRaw]]
    val formTemplateRepo = mock[Repo[FormTemplate]]
    val formRedirectRepo = mock[Repo[FormRedirect]]
    val handlebarsTemplateAlgebra = mock[HandlebarsTemplateAlgebra[FOpt]]
    val handlebarsSchemaAlgebra = mock[HandlebarsSchemaAlgebra[FOpt]]
    val configModule = mock[ConfigModule]
    val gformFrontendConnector = mock[GformFrontendConnector]

    val service = new FormTemplateService(
      formTemplateRepo,
      formTemplateRawRepo,
      formRedirectRepo,
      handlebarsTemplateAlgebra,
      handlebarsSchemaAlgebra,
      gformFrontendConnector,
      configModule
    )

    when(formTemplateRawRepo.replace(formTemplateRaw))
      .thenReturn(EitherT.right[UnexpectedState](Future.successful(())))

    val result = service.save(formTemplateRaw).value.futureValue

    result shouldBe Right(())
    verify(formTemplateRawRepo, times(1)).replace(formTemplateRaw)
  }
}
