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
import munit.FunSuite
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{ mock, never, times, verify, when }
import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.{ JsObject, JsString }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formredirect.FormRedirect
import uk.gov.hmrc.gform.gformfrontend.GformFrontendConnector
import uk.gov.hmrc.gform.handlebarstemplate.{ HandlebarsSchemaAlgebra, HandlebarsTemplateAlgebra }
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId, FormTemplateRaw, FormTemplateRawId }

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success }

class FormTemplateServiceSuite extends FunSuite with ScalaFutures with ExampleData {

  implicit private val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  private case class TestEnv(
    formTemplateRawRepo: Repo[FormTemplateRaw],
    formTemplateRepo: Repo[FormTemplate],
    service: FormTemplateService,
    rawJsObject: JsObject
  )

  private def newTestEnv(): TestEnv = {
    val formTemplateRawRepo = mock(classOf[Repo[FormTemplateRaw]])
    val formTemplateRepo = mock(classOf[Repo[FormTemplate]])
    val formRedirectRepo = mock(classOf[Repo[FormRedirect]])
    val handlebarsTemplateAlgebra = mock(classOf[HandlebarsTemplateAlgebra[FOpt]])
    val handlebarsSchemaAlgebra = mock(classOf[HandlebarsSchemaAlgebra[FOpt]])
    val configModule = mock(classOf[ConfigModule])
    val gformFrontendConnector = mock(classOf[GformFrontendConnector])

    val service = new FormTemplateService(
      formTemplateRepo,
      formTemplateRawRepo,
      formRedirectRepo,
      handlebarsTemplateAlgebra,
      handlebarsSchemaAlgebra,
      gformFrontendConnector,
      configModule
    )

    val rawJsObject = JsObject(
      Seq(
        "_id"      -> JsString("my-form"),
        "formName" -> JsString("My Form"),
        "version"  -> JsString("1")
      )
    )

    TestEnv(formTemplateRawRepo, formTemplateRepo, service, rawJsObject)
  }

  test(
    "FormTemplateService.createE2e should successfully create an e2e form template from an existing form template"
  ) {
    val formTemplateId = FormTemplateId("my-form")

    val env = newTestEnv()
    import env._

    when(formTemplateRawRepo.getDocumentAsJson(formTemplateId.value))
      .thenReturn(Future.successful(rawJsObject))

    when(formTemplateRepo.get(formTemplateId.value))
      .thenReturn(Future.successful(formTemplate))

    when(formTemplateRawRepo.replace(any[FormTemplateRaw]))
      .thenReturn(EitherT.right[UnexpectedState](Future.successful(())))

    when(formTemplateRepo.replace(any[FormTemplate]))
      .thenReturn(EitherT.right[UnexpectedState](Future.successful(())))

    val result = service.createE2e(formTemplateId)

    result.fold(
      _ => fail("Expected successful creation of e2e form template"),
      result => assertEquals(result, ())
    )

    verify(formTemplateRawRepo, times(1)).replace(any[FormTemplateRaw])
    verify(formTemplateRepo, times(1)).replace(any[FormTemplate])
  }

  test(
    "FormTemplateService.createE2e should throw IllegalArgumentException when attempting to create e2e template with ID already ending in -e2e"
  ) {
    val e2eFormTemplateId = FormTemplateId("my-form-e2e")

    val env = newTestEnv()
    import env._

    val resultF = service.createE2e(e2eFormTemplateId).value

    verify(formTemplateRawRepo, never).getDocumentAsJson(any[String])

    resultF.transform {
      case Failure(ex: IllegalArgumentException) =>
        Success(assert(ex.getMessage.contains("is already an e2e form template")))
      case Failure(other)     => Failure(new AssertionError(s"Expected an IllegalArgumentException but got $other"))
      case Success(Left(ex))  => Failure(new AssertionError(s"Expected an exception to be thrown but got Left($ex)"))
      case Success(Right(())) => Failure(new AssertionError("Expected an exception to be thrown"))
    }
  }

  test("FormTemplateService.createE2e should throw NoSuchElementException when original template does not exist") {
    val formTemplateId = FormTemplateId("non-existent-form")

    val env = newTestEnv()
    import env._

    val rawJsObject = JsObject(Seq("_id" -> JsString("non-existent-form")))

    when(formTemplateRawRepo.getDocumentAsJson(formTemplateId.value))
      .thenReturn(Future.successful(rawJsObject))

    when(formTemplateRepo.get(formTemplateId.value))
      .thenReturn(
        Future.failed(new NoSuchElementException(s"Form template with ID ${formTemplateId.value} does not exist"))
      )

    val resultF = service.createE2e(formTemplateId).value

    resultF.transform {
      case Failure(ex: NoSuchElementException) => Success(assert(ex.isInstanceOf[NoSuchElementException]))
      case Failure(other)                      => Failure(new AssertionError(s"Expected a NoSuchElementException but got $other"))
      case Success(Left(ex))                   => Failure(new AssertionError(s"Expected an exception to be thrown but got Left($ex)"))
      case Success(Right(()))                  => Failure(new AssertionError("Expected an exception to be thrown"))
    }
  }

  test("FormTemplateService.createE2e should throw Exception when repository replace fails") {
    val formTemplateId = FormTemplateId("my-form")

    val env = newTestEnv()
    import env._

    when(formTemplateRawRepo.getDocumentAsJson(formTemplateId.value))
      .thenReturn(Future.successful(rawJsObject))

    when(formTemplateRepo.get(formTemplateId.value))
      .thenReturn(Future.successful(formTemplate))

    when(formTemplateRawRepo.replace(any[FormTemplateRaw]))
      .thenReturn(EitherT.left[Unit](Future.successful(UnexpectedState("Database error"))))

    val result = service.createE2e(formTemplateId)

    result.value.map {
      case Left(ex)      => assertEquals(ex, UnexpectedState("Database error"))
      case Right(result) => fail(s"Expected an exception to be thrown but got $result")
    }
  }

  test("FormTemplateService.createE2e should verify JSON _id field is correctly updated with -e2e suffix") {
    val formTemplateId = FormTemplateId("my-form")

    val env = newTestEnv()
    import env._

    var capturedRawTemplate: Option[FormTemplateRaw] = None

    when(formTemplateRawRepo.getDocumentAsJson(formTemplateId.value))
      .thenReturn(Future.successful(rawJsObject))

    when(formTemplateRepo.get(formTemplateId.value))
      .thenReturn(Future.successful(formTemplate))

    when(formTemplateRawRepo.replace(any[FormTemplateRaw]))
      .thenAnswer { invocation =>
        val arg = invocation.getArgument[FormTemplateRaw](0)
        capturedRawTemplate = Some(arg)
        EitherT.right[UnexpectedState](Future.successful(()))
      }

    when(formTemplateRepo.replace(any[FormTemplate]))
      .thenReturn(EitherT.right[UnexpectedState](Future.successful(())))

    val result = service.createE2e(formTemplateId)

    result.fold(
      _ => fail("Expected successful creation of e2e form template"),
      result => {
        assertEquals(result, ())
        assert(capturedRawTemplate.isDefined)
        val capturedJson = capturedRawTemplate.get.value
        assertEquals((capturedJson \ "_id").as[String], "my-form-e2e")
      }
    )
  }

  test("FormTemplateService.get(FormTemplateId) should retrieve a form template by ID") {
    val formTemplateId = FormTemplateId("my-form")

    val env = newTestEnv()
    import env._

    when(formTemplateRepo.get(formTemplateId.value))
      .thenReturn(Future.successful(formTemplate))

    val result = service.get(formTemplateId).futureValue

    assertEquals(result, formTemplate)
  }

  test("FormTemplateService.get(FormTemplateRawId) should retrieve a raw JSON form template by ID") {
    val formTemplateRawId = FormTemplateRawId("my-form")

    val env = newTestEnv()
    import env._

    when(formTemplateRawRepo.getDocumentAsJson(formTemplateRawId.value))
      .thenReturn(Future.successful(rawJsObject))

    val result = service.get(formTemplateRawId).futureValue

    assertEquals(result, FormTemplateRaw(rawJsObject))
  }

  test(
    "FormTemplateService.get(FormTemplateRawId) should fail with a RuntimeException when the stored document is not a JsObject"
  ) {
    val formTemplateRawId = FormTemplateRawId("my-form")

    val env = newTestEnv()
    import env._

    when(formTemplateRawRepo.getDocumentAsJson(formTemplateRawId.value))
      .thenReturn(Future.successful(JsString("not-an-object")))

    val resultF = service.get(formTemplateRawId)

    resultF.transform {
      case Failure(ex: RuntimeException) => Success(assert(ex.getMessage.contains("Expected JsObject type")))
      case Failure(other)                => Failure(new AssertionError(s"Expected a RuntimeException but got $other"))
      case Success(other)                => Failure(new AssertionError(s"Expected a failure but got $other"))
    }
  }

  test("FormTemplateService.find should find a form template by ID") {
    val formTemplateId = FormTemplateId("my-form")

    val env = newTestEnv()
    import env._

    when(formTemplateRepo.find(formTemplateId.value))
      .thenReturn(Future.successful(Some(formTemplate)))

    val result = service.find(formTemplateId).futureValue

    assertEquals(result, Some(formTemplate))
  }

  test("FormTemplateService.getAll should retrieve all raw form templates") {
    val env = newTestEnv()
    import env._

    val rawJs1 = JsObject(Seq("_id" -> JsString("form-1")))
    val rawJs2 = JsObject(Seq("_id" -> JsString("form-2")))
    val templates = List(FormTemplateRaw(rawJs1), FormTemplateRaw(rawJs2))

    when(formTemplateRawRepo.findAll())
      .thenReturn(Future.successful(templates))

    val result = service.getAll().futureValue

    assertEquals(result, templates)
  }

  test("FormTemplateService.save should save a form template raw") {
    val env = newTestEnv()
    import env._

    val formTemplateRaw = FormTemplateRaw(rawJsObject)

    when(formTemplateRawRepo.replace(formTemplateRaw))
      .thenReturn(EitherT.right[UnexpectedState](Future.successful(())))

    val result = service.save(formTemplateRaw).value.futureValue

    assertEquals(result, Right(()))
    verify(formTemplateRawRepo, times(1)).replace(formTemplateRaw)
  }
}
