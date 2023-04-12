/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.builder

import cats.implicits._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.mvc.{ ControllerComponents, Result, Results }
import scala.concurrent.Future
import scala.language.postfixOps
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, FormTemplatesControllerRequestHandler, RequestHandlerAlg }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.concurrent.ExecutionContext

object BuilderSupport {

  def updateSectionStringFields(section: JsValue, sectionStringFields: JsObject): JsValue =
    section.validate(
      (__ \ "fields").json.pickBranch and
        ((__ \ "validators").json.pickBranch orElse Reads.pure(Json.obj())) reduce
    ) match {
      case JsSuccess(fields, _) => sectionStringFields ++ fields
      case _                    => section
    }

  def updateSectionByIndex(
    formTemplateRaw: FormTemplateRaw,
    sectionNumber: Int,
    sectionData: JsObject
  ): JsResult[FormTemplateRaw] = {
    val sectionReads: Reads[JsArray] = Reads.of[JsArray].map { case JsArray(sections) =>
      val section: JsValue = sections(sectionNumber)
      val updatedSection = updateSectionStringFields(section, sectionData)
      val updatedSections = sections.updated(sectionNumber, updatedSection)
      JsArray(updatedSections)
    }

    val res = formTemplateRaw.value.transform((__ \ "sections").json.update(sectionReads))
    res.map(FormTemplateRaw.apply)
  }
}

class BuilderController(controllerComponents: ControllerComponents, formTemplateService: FormTemplateService)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {

  private val requestHandler: RequestHandlerAlg[FOpt] =
    new FormTemplatesControllerRequestHandler(
      formTemplateService.verifyAndSave,
      formTemplateService.save
    ).futureInterpreter

  private def applyUpdateFunction(
    formTemplateRawId: FormTemplateRawId
  )(
    updateFormTemplateRaw: FormTemplateRaw => JsResult[(FormTemplateRaw, Result)]
  ) = {

    val jsResult: Future[JsResult[(FormTemplateRaw, Result)]] = for {
      formTemplateRaw <- formTemplateService.get(formTemplateRawId)
    } yield updateFormTemplateRaw(formTemplateRaw)

    jsResult.flatMap {
      case JsSuccess((templateRaw, result), _) =>
        requestHandler.handleRequest(templateRaw).fold(_.asBadRequest, _ => result)
      case JsError(error) => BadRequest(error.toString).pure[Future]
    }
  }

  private def updateAction(formTemplateRawId: FormTemplateRawId)(
    updateFunction: (FormTemplateRaw, JsObject) => JsResult[(FormTemplateRaw, Result)]
  ) =
    Action.async(parse.json[JsObject]) { implicit request =>
      applyUpdateFunction(formTemplateRawId)(formTemplateRaw => updateFunction(formTemplateRaw, request.body))
    }

  def updateSectionByIndex(formTemplateRawId: FormTemplateRawId, sectionNumber: Int) =
    updateAction(formTemplateRawId) { (formTemplateRaw, requestBody) =>
      BuilderSupport
        .updateSectionByIndex(formTemplateRaw, sectionNumber, requestBody)
        .map(formTemplateRaw => (formTemplateRaw, Results.Ok(formTemplateRaw.value)))
    }

}
