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
import uk.gov.hmrc.gform.core.{ FOpt, _ }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formredirect.FormRedirect
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.concurrent.{ ExecutionContext, Future }
import java.time.Instant

trait FormTemplateAlgebra[F[_]] {
  def get(id: FormTemplateId): F[FormTemplate]
  def find(id: FormTemplateId): F[Option[FormTemplate]]
}

class FormTemplateService(
  formTemplateRepo: Repo[FormTemplate],
  formTemplateRawRepo: Repo[FormTemplateRaw],
  formRedirectRepo: Repo[FormRedirect]
)(implicit
  ec: ExecutionContext
) extends Verifier with Rewriter with SubstituteExpressions with SubstituteBooleanExprs
    with FormTemplateAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  def save(formTemplateRaw: FormTemplateRaw): FOpt[Unit] =
    formTemplateRawRepo.replace(formTemplateRaw)

  def get(id: FormTemplateId): Future[FormTemplate] = formTemplateRepo.get(id.value)

  def find(id: FormTemplateId): Future[Option[FormTemplate]] = formTemplateRepo.find(id.value)

  def get(id: FormTemplateRawId): Future[FormTemplateRaw] =
    formTemplateRawRepo.getDocumentAsJson(id.value).map {
      case jsObject: JsObject => FormTemplateRaw(jsObject)
      case other              => throw new RuntimeException(s"Expected JsObject type, got $other")
    }

  def delete(formTemplateId: FormTemplateId): FOpt[DeleteResults] =
    for {
      _                   <- formRedirectRepo.deleteByFieldName("redirect", formTemplateId.value)
      prodDeleted         <- formTemplateRepo.delete(formTemplateId.value)
      prodSpecimenDeleted <- formTemplateRepo.delete("specimen-" + formTemplateId.value)
      rawDeleted          <- formTemplateRawRepo.delete(formTemplateId.value)
    } yield DeleteResults(prodDeleted, prodSpecimenDeleted, rawDeleted)

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

  def verifyAndSave(
    formTemplate: FormTemplate
  )(
    expressionsContext: ExprSubstitutions
  )(
    booleanExpressionsContext: BooleanExprSubstitutions
  ): FOpt[Unit] = {

    val exprSubstitutionsResolved: Either[UnexpectedState, ExprSubstitutions] = expressionsContext.resolveSelfReferences

    def substitute(template: FormTemplate) =
      for {
        expressionsContextSubstituted <- fromOptA(exprSubstitutionsResolved)
        substitutedFormTemplate = substituteExpressions(template, expressionsContextSubstituted)
        substitutedBooleanExprsFormTemplate =
          substituteBooleanExprs(
            substitutedFormTemplate,
            booleanExpressionsContext,
            expressionsContextSubstituted
          )
        _                   <- verify(substitutedBooleanExprsFormTemplate)(expressionsContext)
        formTemplateUpdated <- rewrite(substitutedBooleanExprsFormTemplate)
      } yield formTemplateUpdated

    for {
      formTemplateToSave <- substitute(formTemplate)
      specimentTemplateSubstituted = substituteExpressions(formTemplate, new SpecimenExprSubstitutions())
      formTemplateSpecimenToSave <- substitute(specimentTemplateSubstituted)
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
