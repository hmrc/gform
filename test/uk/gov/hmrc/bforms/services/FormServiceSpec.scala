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

package uk.gov.hmrc.bforms.services

import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Span }
import play.api.libs.json.{ JsObject, Json }
import uk.gov.hmrc.bforms.{ FindOneCheck, TypeclassFixtures }
import uk.gov.hmrc.bforms.exceptions.InvalidState
import uk.gov.hmrc.bforms.models._
import uk.gov.hmrc.bforms.typeclasses.{ FindOne, Insert, Update }
import scala.concurrent.ExecutionContext.Implicits.global

class FormServiceSpec extends FlatSpec with Matchers with TypeclassFixtures with ScalaFutures with EitherValues with MockFactory {

  implicit override val patienceConfig = PatienceConfig(timeout = scaled(Span(500, Millis)), interval = scaled(Span(150, Millis)))

  val form = Form(FormId("form-id"), FormData(FormTypeId("form-type-id"), "1.0.0", "UTF-8", Seq.empty[FormField]))

  val formTemplateWithOneSection = FormTemplate(
    Json.obj(
      "formTypeId" -> "IPT100",
      "sections" -> Json.arr(
        Json.obj(
          "title" -> "Your details",
          "fields" -> Json.arr(
            Json.obj(
              "id" -> "firstName",
              "label" -> "Your first name",
              "mandatory" -> "true"
            ),
            Json.obj(
              "id" -> "lastName",
              "label" -> "Your last name",
              "mandatory" -> "true"
            )
          )
        )
      )
    )
  )

  val formTemplateWithTwoSections = FormTemplate(
    Json.obj(
      "formTypeId" -> "IPT100",
      "sections" -> Json.arr(
        Json.obj(
          "title" -> "Your details",
          "fields" -> Json.arr(
            Json.obj(
              "id" -> "firstName",
              "label" -> "Your first name",
              "mandatory" -> "true"
            ),
            Json.obj(
              "id" -> "lastName",
              "label" -> "Your last name",
              "mandatory" -> "true"
            )
          )
        ),
        Json.obj(
          "title" -> "Business details",
          "fields" -> Json.arr(
            Json.obj(
              "id" -> "nameOfBusiness",
              "label" -> "Name of business",
              "mandatory" -> "true"
            ),
            Json.obj(
              "id" -> "accountingPeriodStartDate",
              "label" -> "Accounting period start date",
              "mandatory" -> "true"
            ),
            Json.obj(
              "id" -> "accountingPeriodEndDate",
              "label" -> "Accounting period end date",
              "mandatory" -> "true"
            )
          )
        )
      )
    )
  )

  "FormService saveOrUpdate" should "return InvalidState when FormTemplate cannot be found" in {

    val findOneCheck = mock[FindOneCheck]

    implicit val findOneFormTemplate: FindOne[FormTemplate] = FindOneTC
      .response(Option.empty[FormTemplate]) // No FormTemplate will be found
      .callCheck(findOneCheck)
      .withChecks { req: JsObject =>
        req should be(Json.obj("formTypeId" -> "form-type-id", "version" -> "1.0.0"))
      }
    implicit val findOneForm: FindOne[Form] = FindOneTC.notUsed[Form]
    implicit val insertForm: Insert[Form] = InsertTC.notUsed[Form]
    implicit val updateForm: Update[Form] = UpdateTC.notUsed[Form]

    (findOneCheck.call _).expects().once

    val res = FormService.saveOrUpdate(form, SaveOperation)

    futureResult(res.value).left.value should be(InvalidState("""FormTemplate {"formTypeId":"form-type-id","version":"1.0.0"} not found"""))
  }

  it should "return InvalidState when Form cannot be found when trying to Update the form" in {

    val findOneCheck = mock[FindOneCheck]

    implicit val findOneFormTemplate: FindOne[FormTemplate] = FindOneTC.notUsed[FormTemplate]
    implicit val findOneForm: FindOne[Form] = FindOneTC
      .response(Option.empty[Form]) // No Form will be found
      .callCheck(findOneCheck)
      .withChecks { req: JsObject =>
        req should be(Json.obj("_id" -> "form-id"))
      }
    implicit val insertForm: Insert[Form] = InsertTC.notUsed[Form]
    implicit val updateForm: Update[Form] = UpdateTC.notUsed[Form]

    (findOneCheck.call _).expects().once

    val res = FormService.saveOrUpdate(form, UpdateOperation)

    futureResult(res.value).left.value should be(InvalidState("""Form {"_id":"form-id"} not found"""))
  }

  it should "return InvalidState when fields in the Form doesn't match the fields in FormTemplate" in {

    implicit val findOneFormTemplate: FindOne[FormTemplate] = FindOneTC
      .response(Some(formTemplateWithOneSection))
      .noChecks
    implicit val findOneForm: FindOne[Form] = FindOneTC.notUsed[Form]
    implicit val insertForm: Insert[Form] = InsertTC.notUsed[Form]
    implicit val updateForm: Update[Form] = UpdateTC.notUsed[Form]

    val res = FormService.saveOrUpdate(form, SaveOperation)

    futureResult(res.value).left.value should be(
      InvalidState(
        """|Cannot find a section corresponding to the formFields
           |FormFields: Set()
           |Sections: List(List(firstName, lastName))""".stripMargin
      )
    )
  }

  it should "successfuly save form with single section" in {

    val formFields = List(
      FormField("firstName", "John"),
      FormField("lastName", "Doe")
    )

    val res = runSaveTest(formFields, formTemplateWithOneSection, SaveOperation)

    futureResult(res.value).right.value should be(UpdateSuccess)
  }

  it should "successfuly save first section of a form with two sections" in {

    val formFields = List(
      FormField("firstName", "John"),
      FormField("lastName", "Doe")
    )

    val res = runSaveTest(formFields, formTemplateWithTwoSections, SaveOperation)

    futureResult(res.value).right.value should be(UpdateSuccess)
  }

  it should "successfuly save second section of a form with two sections" in {

    val formFields = List(
      FormField("nameOfBusiness", "Foldright"),
      FormField("accountingPeriodStartDate", "1.1.2000"),
      FormField("accountingPeriodEndDate", "1.1.2000")
    )

    val res = runSaveTest(formFields, formTemplateWithTwoSections, SaveOperation)

    futureResult(res.value).right.value should be(UpdateSuccess)
  }

  it should "return InvalidState when mandatory field is empty on Save" in {

    val formFields = List(
      FormField("nameOfBusiness", "Foldright"),
      FormField("accountingPeriodStartDate", "1.1.2000"),
      FormField("accountingPeriodEndDate", "")
    )

    val res = runSaveTest(formFields, formTemplateWithTwoSections, SaveOperation)

    futureResult(res.value).left.value should be(InvalidState("Required fields accountingPeriodEndDate are missing in form submission."))
  }

  it should "allow save incomplete form when needed" in {

    val formFields = List(
      FormField("nameOfBusiness", "Foldright"),
      FormField("accountingPeriodStartDate", "1.1.2000"),
      FormField("accountingPeriodEndDate", "")
    )

    val res = runSaveTest(formFields, formTemplateWithTwoSections, SaveTolerantOperation)

    futureResult(res.value).right.value should be(UpdateSuccess)
  }

  it should "allow update incomplete form when needed" in {

    val formFields = List(
      FormField("nameOfBusiness", "Foldright"),
      FormField("accountingPeriodStartDate", "1.1.2000"),
      FormField("accountingPeriodEndDate", "")
    )

    val res = runUpdateTest(formFields, formTemplateWithTwoSections, UpdateTolerantOperation)

    futureResult(res.value).right.value should be(UpdateSuccess)
  }

  it should "return InvalidState when mandatory field is empty on Update" in {

    val formFields = List(
      FormField("nameOfBusiness", "Foldright"),
      FormField("accountingPeriodStartDate", "1.1.2000"),
      FormField("accountingPeriodEndDate", "")
    )

    val res = runUpdateTest(formFields, formTemplateWithTwoSections, UpdateOperation)

    futureResult(res.value).left.value should be(InvalidState("Required fields accountingPeriodEndDate are missing in form submission."))
  }

  def runSaveTest(formFields: List[FormField], formTemplate: FormTemplate, operation: MongoOperation) = {
    val formToSave = Form(FormId("form-id"), FormData(FormTypeId("form-type-id"), "1.0.0", "UTF-8", formFields))

    implicit val findOneFormTemplate: FindOne[FormTemplate] = FindOneTC
      .response(Some(formTemplate))
      .noChecks
    implicit val findOneForm: FindOne[Form] = FindOneTC.notUsed[Form]

    implicit val insertForm: Insert[Form] = InsertTC
      .response(Right(UpdateSuccess))
      .withChecks { (selector: JsObject, form: Form) =>
        form should be(formToSave)
        selector should be(Json.obj("_id" -> "form-id"))
      }
    implicit val updateForm: Update[Form] = UpdateTC.notUsed[Form]

    FormService.saveOrUpdate(formToSave, operation)

  }

  def runUpdateTest(formFields: List[FormField], formTemplate: FormTemplate, operation: MongoOperation) = {
    val formToSave = Form(FormId("form-id"), FormData(FormTypeId("form-type-id"), "1.0.0", "UTF-8", formFields))

    val formInDb = Form(FormId("form-id"), FormData(FormTypeId("form-type-id"), "1.0.0", "UTF-8", List.empty[FormField]))

    implicit val findOneFormTemplate: FindOne[FormTemplate] = FindOneTC
      .response(Some(formTemplate))
      .noChecks

    implicit val findOneForm: FindOne[Form] = FindOneTC
      .response(Some(formInDb))
      .withChecks { req: JsObject =>
        req should be(Json.obj("_id" -> "form-id"))
      }

    implicit val insertForm: Insert[Form] = InsertTC.notUsed[Form]

    implicit val updateForm: Update[Form] = UpdateTC
      .response(Right(UpdateSuccess))
      .withChecks { (selector: JsObject, form: Form) =>
        form should be(formToSave)
        selector should be(Json.obj("_id" -> "form-id"))
      }

    FormService.saveOrUpdate(formToSave, operation)

  }
}
