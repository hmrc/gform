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

package uk.gov.hmrc.gform.formtemplate

import cats.implicits._
import org.slf4j.LoggerFactory
import play.api.libs.json.JsObject
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.core.{ FOpt, _ }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formredirect.FormRedirect
import uk.gov.hmrc.gform.gformfrontend.GformFrontendConnector
import uk.gov.hmrc.gform.handlebarstemplate.{ HandlebarsSchemaAlgebra, HandlebarsTemplateAlgebra }
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.{ HandlebarsSchemaId, HandlebarsTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DataOutputFormat.HBS
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.{ DataStore, HandlebarsHttpApi, HmrcDms }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DestinationId, Destinations, UploadableConditioning }

import scala.concurrent.{ ExecutionContext, Future }
import java.time.Instant

trait FormTemplateAlgebra[F[_]] {
  def get(id: FormTemplateId): F[FormTemplate]
  def find(id: FormTemplateId): F[Option[FormTemplate]]
}

class FormTemplateService(
  formTemplateRepo: Repo[FormTemplate],
  formTemplateRawRepo: Repo[FormTemplateRaw],
  formRedirectRepo: Repo[FormRedirect],
  handlebarsTemplateAlgebra: HandlebarsTemplateAlgebra[FOpt],
  handlebarsSchemaAlgebra: HandlebarsSchemaAlgebra[FOpt],
  appConfig: AppConfig,
  gformFrontendConnector: GformFrontendConnector
)(implicit
  ec: ExecutionContext
) extends Verifier with Rewriter with SubstituteExpressions with SubstituteBooleanExprs
    with FormTemplateAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  def save(formTemplateRaw: FormTemplateRaw): FOpt[Unit] =
    formTemplateRawRepo.replace(formTemplateRaw)

  def get(id: FormTemplateId): Future[FormTemplate] = formTemplateRepo.get(id.value)

  def find(id: FormTemplateId): Future[Option[FormTemplate]] = formTemplateRepo.find(id.value)

  def find(id: FormTemplateRawId): Future[Option[FormTemplateRaw]] = formTemplateRawRepo.find(id.value)

  def get(id: FormTemplateRawId): Future[FormTemplateRaw] =
    formTemplateRawRepo.getDocumentAsJson(id.value).map {
      case jsObject: JsObject => FormTemplateRaw(jsObject)
      case other              => throw new RuntimeException(s"Expected JsObject type, got $other")
    }

  def getAll(): Future[List[FormTemplateRaw]] = formTemplateRawRepo.findAll()

  def delete(formTemplateId: FormTemplateId): FOpt[DeleteResults] =
    for {
      formRedirectDeleteResult      <- formRedirectRepo.deleteByFieldName("redirect", formTemplateId.value)
      formTemplateDeleteResult      <- formTemplateRepo.delete(formTemplateId.value)
      specimenDeleteResult          <- formTemplateRepo.delete("specimen-" + formTemplateId.value)
      formTemplateRawDeleteResult   <- formTemplateRawRepo.delete(formTemplateId.value)
      formTemplateCacheDeleteResult <- gformFrontendConnector.deleteFormTemplateCache(formTemplateId)
    } yield DeleteResults(
      formTemplateDeleteResult,
      specimenDeleteResult,
      formTemplateRawDeleteResult,
      formRedirectDeleteResult,
      formTemplateCacheDeleteResult
    )

  def list(): Future[List[String]] =
    formTemplateRepo
      .projection("_id")
      .map(_.flatMap { jsValue =>
        (jsValue \ "_id").asOpt[String] match {
          case None =>
            logger.error("Failed to extract _id as a String from json: " + jsValue)
            None
          case some => some
        }
      })

  def getFormTemplateHandlebars(formTemplateId: FormTemplateId): Future[List[HandlebarsTemplateId]] =
    for {
      formTemplate <- formTemplateRepo.get(formTemplateId.value)
      handlebarsPayloadIds = formTemplate.destinations match {
                               case destinationList: Destinations.DestinationList =>
                                 destinationList.destinations.collect { case h: HandlebarsHttpApi =>
                                   HandlebarsTemplateId(s"${formTemplate._id.value}-${h.id.id}")
                                 }
                               case _ => List.empty[HandlebarsTemplateId]
                             }
    } yield handlebarsPayloadIds.sortBy(_.value)

  def verifyAndSave(
    formTemplate: FormTemplate
  )(
    expressionsContext: ExprSubstitutions
  )(
    booleanExpressionsContext: BooleanExprSubstitutions
  ): FOpt[Unit] = {

    val exprSubstitutionsResolved: Either[UnexpectedState, ExprSubstitutions] = expressionsContext.resolveSelfReferences

    val exprBooleanSubstitutionsResolved: Either[UnexpectedState, BooleanExprSubstitutions] =
      booleanExpressionsContext.resolveSelfReferences

    def substitute(template: FormTemplate) =
      for {
        expressionsContextSubstituted        <- fromOptA(exprSubstitutionsResolved)
        booleanExpressionsContextSubstituted <- fromOptA(exprBooleanSubstitutionsResolved)
        substitutedFormTemplateExprs = substituteExpressions(template, expressionsContextSubstituted)
        substitutedFormTemplateBooleanExprs =
          substituteBooleanExprs(
            substitutedFormTemplateExprs,
            booleanExpressionsContextSubstituted,
            expressionsContextSubstituted
          )
        substitutedFormTemplate <- substituteDestinations(substitutedFormTemplateBooleanExprs)
        formTemplateWithPageHeadings = PageHeadingHelper.fillBlankPageHeadings(substitutedFormTemplate)
        handlebarsSchemaIds <- handlebarsSchemaAlgebra.getAllIds
        _                   <- verify(formTemplateWithPageHeadings, appConfig, handlebarsSchemaIds)(expressionsContext)
        formTemplateUpdated <- rewrite(formTemplateWithPageHeadings)
      } yield formTemplateUpdated

    def substituteDestinations(formTemplate: FormTemplate) = {
      val destIds = formTemplate.destinations match {
        case destinationList: Destinations.DestinationList =>
          destinationList.destinations.collect {
            case h: HandlebarsHttpApi if h.payload.isEmpty          => h.id
            case d: DataStore if d.handlebarPayload                 => d.id
            case h: HmrcDms if h.dataOutputFormat.exists(_ === HBS) => h.id
          }
        case _ => List.empty[DestinationId]
      }

      val schemaValidationReqDestIds = formTemplate.destinations match {
        case destinationList: Destinations.DestinationList =>
          destinationList.destinations.collect {
            case d: DataStore if d.validateHandlebarPayload => d.id
          }
        case _ => List.empty[DestinationId]
      }

      for {
        destinationIdsPayloads <- destIds.traverse { destId =>
                                    val templateId = HandlebarsTemplateId(s"${formTemplate._id.value}-${destId.id}")
                                    handlebarsTemplateAlgebra
                                      .get(templateId)
                                      .map(
                                        _.getOrElse(
                                          throw new NoSuchElementException(
                                            s"The ${destId.id} destination is not valid. ${templateId.value} payload not found"
                                          )
                                        )
                                      )
                                      .map(r => destId -> r.payload)
                                  }
        jsonValidators <- schemaValidationReqDestIds.traverse { destId =>
                            val handlebarsSchemaId = HandlebarsSchemaId(formTemplate._id.value)
                            handlebarsSchemaAlgebra
                              .get(handlebarsSchemaId)
                              .map(
                                _.getOrElse(
                                  throw new NoSuchElementException(
                                    s"The ${destId.id} destination is not valid. ${handlebarsSchemaId.value} json schema not found"
                                  )
                                )
                              )
                              .map(r => destId -> r.schema)
                          }

      } yield {

        def getPayload(destinationId: DestinationId, convertSingleQuotes: Option[Boolean]) =
          destinationIdsPayloads.toMap
            .get(destinationId)
            .map(payload => UploadableConditioning.conditionAndValidate(convertSingleQuotes, payload).getOrElse(""))

        if (destIds.size === 0) formTemplate
        else
          formTemplate.copy(destinations = formTemplate.destinations match {
            case destinationList: Destinations.DestinationList =>
              destinationList.copy(destinations = destinationList.destinations.map {
                case h: HandlebarsHttpApi if h.payload.isEmpty =>
                  val payload = getPayload(h.id, h.convertSingleQuotes)
                  h.copy(payload = payload)
                case h: HmrcDms if h.payload.isEmpty =>
                  val payload = getPayload(h.id, h.convertSingleQuotes)
                  h.copy(payload = payload)
                case d: DataStore if d.handlebarPayload =>
                  val payload = getPayload(d.id, d.convertSingleQuotes)
                  if (payload.isEmpty) {
                    throw new Exception(s"Couldn't find handlebars payload for ${d.id}")
                  } else if (d.validateHandlebarPayload) {
                    val jsonSchema = jsonValidators.toMap.get(d.id)

                    if (jsonSchema.isEmpty) {
                      throw new Exception(s"Couldn't find handlebars schema for ${d.id}")
                    } else d.copy(jsonSchema = jsonSchema, payload = payload)
                  } else d.copy(payload = payload)
                case otherDestination => otherDestination
              })
            case otherDestinationList => otherDestinationList
          })
      }
    }

    for {
      formTemplateToSave <- substitute(formTemplate)
      specimenTemplateSubstituted = substituteExpressions(formTemplate, new SpecimenExprSubstitutions())
      formTemplateSpecimenToSave <- substitute(specimenTemplateSubstituted)
      _                          <- formTemplateRepo.replace(mkSpecimen(formTemplateSpecimenToSave))
      _                          <- formRedirectRepo.deleteByFieldName("redirect", formTemplate._id.value)
      _ <- formTemplate.legacyFormIds.fold(success(())) { legacyFormIds =>
             formRedirectRepo.upsertBulk(
               legacyFormIds.map(FormRedirect(_, formTemplate._id, Instant.now, Instant.now)).toList
             )
           }
      res <- formTemplateRepo.replace(formTemplateToSave)
    } yield res
  }
}
