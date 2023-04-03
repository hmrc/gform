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
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.concurrent.ExecutionContext
import scala.language.postfixOps
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.core.{ FOpt, Opt, fromOptA }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Default, Expr, ExpressionOutput, FormCategory, FormTemplate, FormTemplateRaw, SummarySection }

import scala.annotation.nowarn

trait RequestHandlerAlg[F[_]] {
  def handleRequest(templateRaw: FormTemplateRaw): F[Unit]
}

class FormTemplatesControllerRequestHandler[F[_]](
  verifyAndSave: FormTemplate => ExprSubstitutions => BooleanExprSubstitutions => FOpt[Unit],
  save: FormTemplateRaw => FOpt[Unit]
)(implicit ec: ExecutionContext) {

  val futureInterpreter = new RequestHandlerAlg[FOpt] {
    override def handleRequest(templateRaw: FormTemplateRaw): FOpt[Unit] = {

      val expressionsContextOpt: Opt[ExprSubstitutions] = ExprSubstitutions.from(templateRaw)

      val booleanExpressionsContextOpt: Opt[BooleanExprSubstitutions] =
        BooleanExprSubstitutions.from(templateRaw)

      val formTemplateOpt: Opt[FormTemplate] = FormTemplate
        .transformAndReads(templateRaw.value)
        .fold(errors => UnexpectedState(errors.toString()).asLeft, valid => valid.asRight)

      val formTemplateWithSubstitutions = for {
        expressionsContext        <- expressionsContextOpt
        booleanExpressionsContext <- booleanExpressionsContextOpt
        formTemplate              <- formTemplateOpt
        expressionsOutput         <- substituteExpressionsOutput(formTemplate.expressionsOutput, expressionsContext)
      } yield {
        val email = expressionsContext.expressions.get(ExpressionId("email"))
        (
          formTemplate.copy(emailExpr = email, expressionsOutput = expressionsOutput),
          expressionsContext,
          booleanExpressionsContext
        )
      }

      processAndPersistTemplate(formTemplateWithSubstitutions, templateRaw.lowerCaseId)
    }
  }

  private def substituteExpressionsOutput(
    maybeExpressionsOutput: Option[ExpressionOutput],
    exprSubstitutions: ExprSubstitutions
  ): Opt[Option[ExpressionOutput]] = maybeExpressionsOutput.traverse[Opt, ExpressionOutput] { expressionOutput =>
    exprSubstitutions.resolveSelfReferences.flatMap { resolveSelfReferences =>
      val lookup = resolveSelfReferences.expressions
      expressionOutput.lookup.toList
        .traverse { case (expressionId, _) =>
          lookup
            .get(expressionId)
            .fold[Opt[(ExpressionId, Expr)]](
              Left(
                UnexpectedState(
                  s"ExpressionId: '${expressionId.id}' defined in expressionsOutput doesn't exist in expressions"
                )
              )
            ) { expr =>
              val exprSubs = ExprSubstituter.substituteExpr(exprSubstitutions, expr)
              Right(expressionId -> exprSubs)
            }
        }
        .map(tuples => ExpressionOutput(tuples.toMap))
    }
  }

  private def processAndPersistTemplate(
    formTemplateOpt: Opt[(FormTemplate, ExprSubstitutions, BooleanExprSubstitutions)],
    templateRaw: FormTemplateRaw
  ): FOpt[Unit] =
    for {
      ft <- fromOptA(formTemplateOpt)
      _  <- verifyAndSave(ft._1)(ft._2)(ft._3)
      _  <- save(templateRaw)
    } yield ()
}

object FormTemplatesControllerRequestHandler {

  val onlyOneOfDestinationsAndPrintSection = JsError(
    """One and only one of FormTemplate.{destinations, printSection} must be defined."""
  )

  val avoidAcknowledgementForPrintSection = JsError("""Avoid acknowledgement section in case of print section.""")

  val avoidDeclarationForPrintSection = JsError("""Avoid declaration section in case of print section.""")

  val mandatoryAcknowledgementForDestinationSection = JsError(
    """Acknowledgement section is mandatory in case of destination section."""
  )

  val invalidDmsFormId = (id: String) =>
    s"For Destination $id, dmsFormId should have minimum 1 and maximum 12 characters."

  val missingDmsFormId = (id: String) => s"For Destination $id, dmsFormId is missing."

  private val noTemplateId: Reads[JsObject] =
    Reads.failed("Template field _id must be provided and it must be a String")

  private def list[A <: JsValue](reads: Reads[A]): Reads[JsArray] =
    Reads.list(reads).map(JsArray.apply)

  def normaliseJSON(jsonValue: JsValue): JsResult[JsObject] = {

    val allowedFileTypes =
      (__ \ Symbol("allowedFileTypes")).json
        .copyFrom(
          (__ \ Symbol("allowedFileTypes")).json.pick orElse Reads.pure(
            JsArray(FileInfoConfig.allAllowedFileTypes.fileExtensions.map(e => JsString(e)).toList)
          )
        )

    val drmValue =
      (__ \ Symbol("draftRetrievalMethod") \ Symbol("value")).json
        .copyFrom((__ \ Symbol("draftRetrievalMethod")).json.pick orElse Reads.pure(JsString("onePerUser")))

    val drmShowContinueOrDeletePage =
      (__ \ Symbol("draftRetrievalMethod") \ Symbol("showContinueOrDeletePage")).json
        .copyFrom((__ \ Symbol("showContinueOrDeletePage")).json.pick orElse Reads.pure(JsString("true")))

    val ensureDisplayHMRCLogo =
      (__ \ Symbol("displayHMRCLogo")).json
        .copyFrom((__ \ Symbol("displayHMRCLogo")).json.pick orElse Reads.pure(JsFalse))

    val ensureOriginalId = (__ \ Symbol("originalId")).json.copyFrom((__ \ "_id").json.pick) orElse noTemplateId

    val lowerCaseId: Reads[JsObject] = (__ \ Symbol("_id")).json.copyFrom(
      (__ \ Symbol("_id")).json
        .pick[JsString]
        .map(jsString => JsString(jsString.value.toLowerCase))
        .widen
    ) orElse noTemplateId

    val pruneShowContinueOrDeletePage = (__ \ Symbol("showContinueOrDeletePage")).json.prune

    val ensureFormCategory =
      (__ \ Symbol("formCategory")).json
        .copyFrom((__ \ Symbol("formCategory")).json.pick orElse Reads.pure(JsString("default")))

    val ensureLanguages =
      (__ \ Symbol("languages")).json
        .copyFrom((__ \ Symbol("languages")).json.pick orElse Reads.pure(Json.arr("en")))

    val formCategoryReads: Reads[FormCategory] = (__ \ Symbol("formCategory")).json.pick
      .flatMap { jsValue =>
        val formCategoryJsRsult: JsResult[FormCategory] = Reads.of[FormCategory].reads(jsValue)
        Reads.pure(formCategoryJsRsult.getOrElse(Default))
      }
      .orElse(Reads.pure(Default))

    val defaultSummarySection: Reads[JsValue] =
      formCategoryReads.map(formCategory => SummarySection.defaultJson(formCategory))

    val ensureSummarySection =
      (__ \ Symbol("summarySection")).json
        .copyFrom((__ \ Symbol("summarySection")).json.pick orElse defaultSummarySection)

    val ensureParentFormSubmissionRefs =
      (__ \ Symbol("parentFormSubmissionRefs")).json
        .copyFrom((__ \ Symbol("parentFormSubmissionRefs")).json.pick orElse Reads.pure(Json.arr()))

    val destinationsOrPrintSection =
      (__ \ Symbol("destinations")).json
        .copyFrom(
          (__ \ Symbol("destinations")).json.pick orElse (__ \ "printSection").json.pick orElse Reads.pure(JsString(""))
        )

    val prunePrintSection = (__ \ Symbol("printSection")).json.prune

    val transformAcknowledgementSection = (jsValue: JsValue) => {
      val jsObject = jsValue.as[JsObject]
      val displayFeedbackLinkValueOrDefault: JsValue = jsObject.value.getOrElse("displayFeedbackLink", JsTrue)
      jsObject ++ Json.obj("displayFeedbackLink" -> displayFeedbackLinkValueOrDefault)
    }

    val transformAndMoveAcknowledgementSection =
      (__ \ Symbol("destinations") \ Symbol("acknowledgementSection")).json
        .copyFrom((__ \ Symbol("acknowledgementSection")).json.pick.map(transformAcknowledgementSection)) orElse Reads
        .pure(
          Json.obj()
        )

    val determineFormKind: Reads[JsObject] = (__ \ Symbol("sections") \ 0 \ Symbol("tasks")).json.pick
      .map(_ => JsString("taskList"))
      .orElse(Reads.pure(JsString("classic")))
      .flatMap { formKind =>
        (__ \ Symbol("formKind") \ Symbol("type")).json.put(formKind)
      }

    val moveSections =
      (__ \ Symbol("formKind") \ Symbol("sections")).json
        .copyFrom((__ \ Symbol("sections")).json.pick) and (__ \ "sections").json.prune reduce
    @nowarn
    def getDownField(fieldName: String, json: JsValue): JsObject =
      (json \ fieldName) match {
        case JsDefined(field) => Json.obj(fieldName -> field)
        case JsUndefined()    => Json.obj()
      }

    val choicesFieldUpdater: Reads[JsValue] = Reads { json =>
      json match {
        case JsObject(_) =>
          val includeIfField = getDownField("includeIf", json)
          val valueField = getDownField("value", json)
          val hintField = getDownField("hint", json)
          val dynamicField = getDownField("dynamic", json)
          val enField = getDownField("en", json)
          val cyField = getDownField("cy", json)

          val labelField: JsObject = Json.obj("label" -> (enField ++ cyField))

          val newJson: JsObject = includeIfField ++ valueField ++ hintField ++ dynamicField ++ labelField

          JsSuccess(newJson)

        case otherwise => JsSuccess(Json.obj("label" -> otherwise))
      }

    }

    val updateChoicesField = (__ \ Symbol("choices")).json.update(list(choicesFieldUpdater))

    val choicesUpdater: Reads[JsValue] = Reads { json =>
      json \ "type" match {
        case JsDefined(JsString(tpe)) if tpe === "choice" =>
          json \ "format" match {
            case JsDefined(JsString("yesno")) => JsSuccess(json)
            case _                            => json.transform(updateChoicesField)
          }
        case _ => JsSuccess(json)
      }
    }

    def revealingChoicesUpdater: Reads[JsValue] = Reads { json =>
      json \ "type" match {
        case JsDefined(JsString("revealingChoice")) =>
          json.transform(
            (__ \ Symbol("revealingFields")).json.update(
              list(list(choicesUpdater andThen revealingChoicesUpdater))
            ) andThen updateChoicesField
          )
        case _ => JsSuccess(json)
      }
    }

    val fileUploadUpdater: Reads[JsValue] = Reads { json =>
      jsonValue \ "objectStore" match {
        case JsDefined(JsBoolean(bool)) if bool =>
          json \ "type" match {
            case JsDefined(JsString("file")) =>
              json.validate(__.json.update((__ \ Symbol("service")).json.put(JsString("upscan"))))
            case _ => JsSuccess(json)
          }
        case _ => JsSuccess(json)
      }
    }

    val fieldsReads: Reads[JsObject] =
      (__ \ Symbol("fields")).json
        .update(list(choicesUpdater andThen revealingChoicesUpdater andThen fileUploadUpdater))

    val regularFieldsOrAddToListFieldsReads =
      (__ \ Symbol("pages")).json.update(list(fieldsReads)) orElse fieldsReads

    val regularFieldsOrAddToListFieldsReadsTaskList =
      (__ \ Symbol("tasks")).json.update(list((__ \ "sections").json.update(list(regularFieldsOrAddToListFieldsReads))))

    val transformChoices: Reads[JsValue] = Reads { json =>
      val updateSections: Reads[JsObject] =
        (__ \ Symbol("sections")).json.update(
          list(regularFieldsOrAddToListFieldsReads) orElse list(regularFieldsOrAddToListFieldsReadsTaskList)
        )
      json.transform(updateSections) orElse JsSuccess(json)
    }

    val transformAddAnotherQuestion: Reads[JsValue] = Reads { json =>
      val updateSections: Reads[JsObject] =
        (__ \ Symbol("sections")).json.update(of[JsArray].map { case JsArray(arr) =>
          JsArray(
            arr.map(item =>
              item
                .transform((__ \ Symbol("addAnotherQuestion")).json.update(choicesUpdater))
                .getOrElse(item)
            )
          )
        })

      json.transform(updateSections) orElse JsSuccess(json)
    }

    val transformConfirmationQuestion: Reads[JsValue] = Reads { json =>
      val questionReads: Reads[JsObject] =
        (__ \ Symbol("confirmation") \ Symbol("question")).json.update(choicesUpdater)
      val updatePageQuestions: Reads[JsObject] =
        (__ \ Symbol("sections")).json.update(of[JsArray].map { case JsArray(arr) =>
          JsArray(
            arr.map(item =>
              item
                .transform((__ \ Symbol("pages")).json.update(of[JsArray].map { case JsArray(arr2) =>
                  JsArray(arr2.map(item2 => item2.transform(questionReads).getOrElse(item2)))
                }))
                .getOrElse(item)
            )
          )
        })
      val updateSectionQuestions: Reads[JsObject] =
        (__ \ Symbol("sections")).json.update(of[JsArray].map { case JsArray(arr) =>
          JsArray(
            arr.map(item =>
              item
                .transform(questionReads)
                .getOrElse(item)
            )
          )
        })

      json.transform(updatePageQuestions andThen updateSectionQuestions) orElse JsSuccess(json)
    }

    val transformDestinations: Reads[JsValue] = Reads { json =>
      @nowarn
      val transformIncludeIfs: Reads[JsValue] = Reads { json =>
        json \ "includeIf" match {
          case JsDefined(JsString(_)) => JsSuccess(json)
          case JsUndefined()          => json.validate(__.json.update((__ \ Symbol("includeIf")).json.put(JsString("true"))))
        }
      }
      json.transform(j =>
        j.validate(of[JsArray].map { case JsArray(arr) =>
          JsArray(
            arr.map(item => item.transform(transformIncludeIfs).getOrElse(item))
          )
        })
      ) orElse JsSuccess(json)
    }

    val moveDestinations =
      (__ \ Symbol("destinations") \ Symbol("destinations")).json
        .copyFrom((__ \ Symbol("destinations")).json.pick.map { dJson =>
          dJson.transform(transformDestinations).getOrElse(dJson)
        }) orElse Reads.pure(Json.obj())

    val moveDeclarationSection =
      (__ \ Symbol("destinations") \ Symbol("declarationSection")).json
        .copyFrom((__ \ Symbol("declarationSection")).json.pick.map { dsJson =>
          dsJson.transform(fieldsReads).getOrElse(dsJson)
        }) orElse Reads.pure(Json.obj())

    val pruneAcknowledgementSection = (__ \ Symbol("acknowledgementSection")).json.prune

    val pruneDeclarationSection = (__ \ Symbol("declarationSection")).json.prune

    val sectionValidations: JsResult[Unit] =
      (
        (jsonValue \ "destinations").toOption,
        (jsonValue \ "printSection").toOption,
        (jsonValue \ "acknowledgementSection").toOption,
        (jsonValue \ "declarationSection").toOption
      ) match {
        case (Some(_), Some(_), _, _)          => onlyOneOfDestinationsAndPrintSection
        case (None, None, _, _)                => onlyOneOfDestinationsAndPrintSection
        case (Some(_), None, None, _)          => mandatoryAcknowledgementForDestinationSection
        case (None, Some(_), Some(_), _)       => avoidAcknowledgementForPrintSection
        case (None, Some(_), _, Some(_))       => avoidDeclarationForPrintSection
        case (Some(_), None, Some(_), Some(_)) => JsSuccess(())
        case (None, Some(_), None, None)       => JsSuccess(())
        case (Some(_), None, _, None)          => JsSuccess(())
      }

    val dmsFormIdValidations: JsResult[Unit] =
      (jsonValue \ "destinations").toOption match {
        case Some(id) =>
          val errorMessages = id
            .as[JsArray]
            .value
            .map { jv =>
              ((jv \ "id").as[String], (jv \ "type").as[String], (jv \ "dmsFormId").asOpt[String].map(_.trim))
            }
            .flatMap {
              // format: off
              case (_, "hmrcDms", Some(dmsFormId))
                  if dmsFormId.length > 0 && dmsFormId.length <= 12  => None
              case (id, "hmrcDms", Some(_))                          => Some(invalidDmsFormId(id))
              case (id, "hmrcDms", None)                             => Some(missingDmsFormId(id))
              case _                                                 => None
              // format: on
            }

          if (errorMessages.isEmpty)
            JsSuccess(())
          else
            JsError(errorMessages.mkString(" "))

        case None => JsSuccess(())
      }

    sectionValidations andKeep dmsFormIdValidations andKeep jsonValue.transform(
      pruneShowContinueOrDeletePage andThen
        pruneAcknowledgementSection andThen
        prunePrintSection andThen
        transformChoices andThen
        transformAddAnotherQuestion andThen
        transformConfirmationQuestion andThen
        moveSections andThen
        pruneDeclarationSection and
        allowedFileTypes and
        drmValue and
        drmShowContinueOrDeletePage and
        ensureOriginalId and
        lowerCaseId and
        ensureDisplayHMRCLogo and
        ensureFormCategory and
        ensureLanguages and
        ensureSummarySection and
        ensureParentFormSubmissionRefs and
        destinationsOrPrintSection and
        transformAndMoveAcknowledgementSection and
        moveDestinations and
        determineFormKind and
        moveDeclarationSection reduce
    )
  }
}
