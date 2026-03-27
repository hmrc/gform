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

package uk.gov.hmrc.gform.handlebarstemplate

import cats.implicits.catsSyntaxApplicativeId
import com.github.jknack.handlebars.HandlebarsException
import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, RequestHandlerAlg }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HandlebarsTemplateProcessorModel, TemplateType, UploadableConditioning }
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, RealHandlebarsTemplateProcessor }

import scala.concurrent.{ ExecutionContext, Future }

class HandlebarsTemplateController(
  controllerComponents: ControllerComponents,
  handlebarsTemplateAlgebra: HandlebarsTemplateAlgebra[Future],
  handler: RequestHandlerAlg[FOpt],
  formTemplateService: FormTemplateService
)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {
  private val logger = LoggerFactory.getLogger(getClass)

  def upsert(handlebarsTemplateId: HandlebarsTemplateId) = Action.async(parse.text) { implicit request =>
    val handleBarsTemplate = HandlebarsTemplate(handlebarsTemplateId, request.body)

    val formTemplateIdStr = handlebarsTemplateId.value.replaceAll("-(\\w+)$", "")
    val formTemplateRawId = FormTemplateRawId(formTemplateIdStr)
    for {
      formTemplateRawOpt <- formTemplateService.get(formTemplateRawId).map(Some(_)).recover {
                              // do not reload template if this is the first upsert
                              case _: NoSuchElementException => None
                            }
      formTemplateOpt <- formTemplateService.get(FormTemplateId(formTemplateIdStr)).map(Some(_)).recover {
                           case _: NoSuchElementException => None
                         }
      _ <- formTemplateOpt match {
             case Some(formTemplate) => validateHandlebarsTemplate(handleBarsTemplate, formTemplate)
             case None =>
               logger.warn(
                 s"Form template not found for Handlebars template: ${handlebarsTemplateId.value}. Skipping validation."
               )
               Future.successful(())
           }
      _ <- handlebarsTemplateAlgebra.save(handleBarsTemplate)
      _ <- formTemplateRawOpt match {
             case Some(formTemplateRaw) => doTemplateUpsert(formTemplateRaw)
             case None                  => Future.successful(())
           }
    } yield NoContent
  }

  private def validateHandlebarsTemplate(
    handlebarsTemplate: HandlebarsTemplate,
    formTemplate: FormTemplate
  ): Future[Unit] = {
    import HandlebarsValidationHelpers._
    logger.info(
      s"Validating Handlebars template for form template: ${formTemplate._id}, handlebars template id: ${handlebarsTemplate._id}"
    )

    // First attempt to validate the template by extracting tokens and checking against form fields
    val tokens: Set[String] = extractTokensFromPayload(handlebarsTemplate.payload) -- knownHelpers
    val syntheticFields: Set[String] = extractSyntheticTokens(handlebarsTemplate.payload)
    val allValidFormFields: Set[String] = getAllFormFields(formTemplate).toSet ++ syntheticFields

    val missingFields: Set[String] = tokens -- allValidFormFields
    if (missingFields.nonEmpty) {
      logger.error(
        s"Handlebars template validation failed for ${handlebarsTemplate._id.value}: Missing fields (or incorrect helpers) in form template: ${missingFields
          .mkString(", ")}"
      )
      return Future.failed(
        new IllegalArgumentException(
          s"Invalid Handlebars template: Missing fields (or incorrect helpers) in form template: ${missingFields.mkString(", ")}"
        )
      )
    }

    try {
      // Now attempt to process the template with a real Handlebars engine to catch any syntax errors or other issues
      val focussedTree: FocussedHandlebarsModelTree = mkFocusedTree(formTemplate)
      val conditionedPayload: String =
        UploadableConditioning.conditionAndValidate(Some(true), handlebarsTemplate.payload).getOrElse("")

      val _: String = RealHandlebarsTemplateProcessor(
        conditionedPayload,
        HandlebarsTemplateProcessorModel.empty,
        focussedTree,
        TemplateType.JSON
      )
      logger.info(
        s"Handlebars template validation successful for ${handlebarsTemplate._id.value}"
      )
      ().pure[Future]
    } catch {
      case e: HandlebarsException =>
        logger.error(s"Handlebars template validation failed for ${handlebarsTemplate._id.value}: ${e.getMessage}")
        Future.failed(new IllegalArgumentException(s"Invalid Handlebars template: ${e.getMessage}"))
    }
  }

  private def doTemplateUpsert(formTemplateRaw: FormTemplateRaw) =
    handler.handleRequest(formTemplateRaw, updateHistory = true).fold(_.asBadRequest, _ => NoContent)

  def getRaw(handlebarsTemplateId: HandlebarsTemplateId) = Action.async { _ =>
    handlebarsTemplateAlgebra
      .get(handlebarsTemplateId)
      .map(handlebarsPayload => Ok(handlebarsPayload.map(_.payload).getOrElse("")).as("text/plain"))
  }

  def get(handlebarsTemplateId: HandlebarsTemplateId) = Action.async { _ =>
    handlebarsTemplateAlgebra
      .get(handlebarsTemplateId)
      .map(handlebarsPayload => Ok(Json.toJson(handlebarsPayload.map(_.payload))))
  }

  def delete(handlebarsTemplateId: HandlebarsTemplateId) = Action.async { _ =>
    handlebarsTemplateAlgebra
      .delete(handlebarsTemplateId)
      .map(res => Ok(Json.toJson(res)))
  }

  def all = Action.async { implicit request =>
    logger.info(s"HandlebarsTemplateController.all, ${loggingHelpers.cleanHeaders(request.headers)}")
    handlebarsTemplateAlgebra.getAll.asOkJson
  }
}
