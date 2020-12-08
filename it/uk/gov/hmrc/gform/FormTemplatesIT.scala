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
package uk.gov.hmrc.gform

import akka.http.scaladsl.model.StatusCodes
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate

import scala.concurrent.ExecutionContext.Implicits.global

class FormTemplatesIT extends ITSpec with ScalaFutures with FormTemplatesSupport with BeforeAndAfterEach {

  implicit val defaultPatience: PatienceConfig =
    PatienceConfig(timeout = Span(10, Seconds), interval = Span(500, Millis))

  override protected def afterEach(): Unit = {
    formTemplateRepo.removeAll().futureValue
    formTemplateRawRepo.removeAll().futureValue
    ()
  }

  "POST /formtemplates" should "insert the template into mongo db" in {

    Given("I POST a form template")
    val result = postTemplate

    Then("The form template should be saved successfully")
    result.status shouldBe StatusCodes.NoContent.intValue
    result.body shouldBe ""

    val formTemplates = formTemplateRepo.findAll().futureValue
    formTemplates.size shouldBe 2
    formTemplates.map(_._id.value) shouldBe List("specimen-BASIC", "BASIC")
    assertBasicFormTemplate(formTemplates.head)
  }

  "GET /formtemplates" should "return all form templates" in {
    Given("I POST a form template")
    postTemplate

    When("I get all form templates")
    val result = getAllTemplates

    Then("I receive the list of all form templates")
    result.status shouldBe StatusCodes.OK.intValue
    Json.parse(result.body).as[List[String]] shouldBe List("specimen-BASIC", "BASIC")
  }

  "GET /formtemplates/:formTemplateId" should "return the requested template" in {
    Given("I POST a form template")
    postTemplate

    When("I get the form template by id")
    val result = getTemplate

    Then("I receive form template")
    result.status shouldBe StatusCodes.OK.intValue
    assertBasicFormTemplate(Json.parse(result.body).as[FormTemplate])
  }

  "GET /formtemplates/:formTemplateId/raw" should "return the requested template in raw format" in {
    Given("I POST a form template")
    postTemplate

    When("I get the form template by id")
    val result = getTemplateRaw

    Then("I receive form template")
    result.status shouldBe StatusCodes.OK.intValue
    Json.parse(result.body).as[JsObject] shouldBe basicFormTemplate
  }

  "DELETE /formtemplates/:formTemplateId" should "delete the requested template" in {
    Given("I POST a form template")
    postTemplate

    When("I delete the form template by id")
    val result = deleteTemplate

    Then("The template should be deleted successfully")
    result.status shouldBe StatusCodes.NoContent.intValue
    result.body shouldBe ""
    getTemplate.status shouldBe StatusCodes.NotFound.intValue
  }
}
