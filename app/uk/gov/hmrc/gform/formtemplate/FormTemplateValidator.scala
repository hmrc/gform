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

import cats.Monoid
import cats.data.NonEmptyList
import cats.implicits._
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.core.ValidationResult.{ BooleanToValidationResultSyntax, validationResultMonoid }
import uk.gov.hmrc.gform.core.{ Invalid, Valid, ValidationResult }
import uk.gov.hmrc.gform.formtemplate.FormTemplateValidatorHelper._
import uk.gov.hmrc.gform.models.constraints.ReferenceInfo._
import uk.gov.hmrc.gform.models.constraints.{ AddressLensChecker, FunctionsChecker, MutualReferenceChecker, ReferenceInfo }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.Attribute
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Dynamic.DataRetrieveBased
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.DependencyGraph._

import java.time.LocalDate
import scala.Function.const
import scala.annotation.nowarn
import scala.util.{ Failure, Success, Try }

object FormTemplateValidator {

  def validateLowercaseIds(
    formTemplate: FormTemplate
  ): ValidationResult = {
    val _id = formTemplate._id.value
    val legacyFormIds = formTemplate.legacyFormIds.fold(List.empty[String])(_.map(_.value).toList)
    if (_id =!= _id.toLowerCase) {
      Invalid(
        "_id must be lowercase. Found: " + _id
      ) // TODO remove lowercasing logic from FormTemplateRaw for this to have an effect
    } else if (legacyFormIds =!= legacyFormIds.map(_.toLowerCase)) {
      Invalid("legacyFormIds must be lowercase. Found: " + legacyFormIds.mkString(", "))
    } else {
      Valid
    }
  }

  def fieldIds(sections: List[Section]): List[FormComponentId] =
    indexedFieldIds(sections).map(_._1) ::: SectionHelper.addToListIds(sections).map(_.id)

  private def indexedFieldIds(sections: List[Section]): List[(FormComponentId, Int)] = indexedFields(sections).map {
    case (a, b) => a.id -> b
  }

  private def indexedFields(sections: List[Section]): List[(FormComponent, Int)] =
    SectionHelper.pages(sections).zipWithIndex.flatMap { case (section, idx) =>
      val standardFields = (section.confirmation.map(_.question).toList ++ section.fields).map(_ -> idx)
      val subFields = section.fields
        .map(_.`type`)
        .collect {
          case Group(fields, _, _, _, _)   => fields.map(_ -> idx)
          case RevealingChoice(options, _) => options.toList.flatMap(_.revealingFields.map(_ -> idx))
        }

      standardFields ::: subFields.flatten
    }

  def someFieldsAreDefinedMoreThanOnce(duplicates: Set[FormComponentId]) =
    s"Some FieldIds are defined more than once: ${duplicates.toList.sortBy(_.value).map(_.value)}"

  def validateUniqueFields(sectionsList: List[Section], expressionIds: List[ExpressionId]): ValidationResult = {
    val ids: List[FormComponentId] = fieldIds(sectionsList) ::: expressionIds.map(e => FormComponentId(e.id))

    val duplicates = ids.groupBy(identity).collect { case (fId, List(_, _, _*)) => fId }.toSet
    duplicates.isEmpty.validationResult(someFieldsAreDefinedMoreThanOnce(duplicates))
  }

  def validateUniquePageIds(sectionsList: List[Section]): ValidationResult = {
    val pageIds: List[PageId] =
      sectionsList.flatMap(
        _.fold(_.page.id.toList)(_.page.id.toList)(p =>
          p.defaultPage.flatMap(_.id).toList ++ p.pages.toList.flatMap(_.id)
        )
      )
    val duplicateIds = pageIds.groupBy(identity).collect {
      case (_, ids) if ids.size > 1 => ids.head
    }
    duplicateIds.isEmpty.validationResult(
      s"Some page ids are defined more than once: ${duplicateIds.toList.sortBy(_.id).map(_.id).mkString(",")}"
    )
  }

  def validateUniqueTaskIds(formTemplate: FormTemplate): ValidationResult = {
    val taskIds: List[TaskId] =
      formTemplate.formKind.fold(classic => List.empty[TaskId])(taskList =>
        taskList.sections.toList.flatMap(_.tasks.toList.flatMap(_.id))
      )
    val duplicateIds = taskIds.groupBy(identity).collect {
      case (_, ids) if ids.size > 1 => ids.head
    }
    duplicateIds.isEmpty.validationResult(
      s"Some task ids are defined more than once: ${duplicateIds.toList.sortBy(_.id).map(_.id).mkString(",")}"
    )
  }

  def validateInstructions(pages: List[Page]): ValidationResult =
    if (!pages.flatMap(_.instruction).flatMap(_.name).forall(_.allNonEmpty)) {
      Invalid("One or more sections have instruction attribute with empty names")
    } else if (!pages.flatMap(_.instruction.flatMap(_.order)).forall(_ >= 0)) {
      Invalid("One or more sections have instruction attribute with negative order")
    } else if (
      pages.flatMap(_.instruction.flatMap(_.order)).distinct.size != pages
        .flatMap(_.instruction.flatMap(_.order))
        .size
    ) {
      Invalid("One or more sections have instruction attribute with duplicate order value")
    } else if (!pages.flatMap(_.fields).flatMap(_.instruction).flatMap(_.name).forall(_.allNonEmpty)) {
      Invalid("One or more section fields have instruction attribute with empty names")
    } else if (!pages.flatMap(_.fields).flatMap(_.instruction.flatMap(_.order)).forall(_ >= 0)) {
      Invalid("One or more section fields have instruction attribute with negative order")
    } else if (
      pages.forall(p =>
        p.fields.flatMap(_.instruction.flatMap(_.order)).distinct.size != p.fields
          .flatMap(_.instruction.flatMap(_.order))
          .size
      )
    ) {
      Invalid("All fields within sections that have instruction defined, must have a unique order value")
    } else {
      Valid
    }

  def validateCsvCountryCountCheck(formTemplate: FormTemplate): ValidationResult = {
    val allExprs: List[ExprWithPath] = FormTemplate.leafExprs.exprs(TemplatePath.root, formTemplate)
    val results = allExprs.collect { case ExprWithPath(path, CsvCountryCountCheck(fcId, _, _)) =>
      val fc = SectionHelper.addToListFormComponents(formTemplate.formKind.allSections).find(_.id == fcId)
      val result: ValidationResult = fc
        .map {
          case IsCountryLookup(fc) => Valid
          case _                   => Invalid(s"$path: $fcId is not a country lookup component")
        }
        .getOrElse(Invalid(s"$path: $fcId is not within AddToList component"))
      result
    }
    Monoid.combineAll(results)
  }

  def validateInvalidReferences(
    formTemplate: FormTemplate,
    allExprs: List[ExprWithPath],
    expressionIds: List[ExpressionId]
  ): ValidationResult = {

    val allPageIds: List[PageId] =
      formTemplate.formKind.allSections.flatMap(
        _.fold(_.page.id.toList)(_.page.id.toList)(p =>
          p.pageId :: (p.defaultPage.flatMap(_.id).toList ++ p.pages.toList.flatMap(_.id))
        )
      )

    val allFcIds: Set[FormComponentId] = formTemplate.formKind.allSections
      .collect {
        case s: Section.NonRepeatingPage => s.page.allFormComponentIds.toSet
        case s: Section.RepeatingPage    => s.page.allFormComponentIds.toSet
        case s: Section.AddToList =>
          (s.addAnotherQuestion.id :: s.pages.toList
            .flatMap(_.allFormComponentIds) ::: s.fields.map(_.toList.map(_.id)).getOrElse(Nil)).toSet
      }
      .toSet
      .flatten

    val allChoiceIds: Set[FormComponentId] = formTemplate.formComponents { case fc @ IsChoice(_) =>
      fc.id
    }.toSet

    val allTextAreaIds: Set[FormComponentId] = formTemplate.formComponents { case fc @ IsTextArea(_) =>
      fc.id
    }.toSet

    val allChoiceCheckboxIds: Set[FormComponentId] = formTemplate.formComponents {
      case fc @ IsChoice(choice: Choice) if choice.`type` === Checkbox =>
        fc.id
    }.toSet

    val allRevealingChoiceCheckboxIds: Set[FormComponentId] = formTemplate.formComponents {
      case fc @ IsRevealingChoice(rChoice: RevealingChoice) if rChoice.multiValue === true =>
        fc.id
    }.toSet

    val allTaskIds: Set[TaskId] =
      formTemplate.formKind.fold(classic => Set.empty[TaskId])(taskList =>
        taskList.sections.toList.flatMap(_.tasks.toList.flatMap(_.id)).toSet
      )

    val fcIdsWithExprs: Set[FormComponentId] = allFcIds ++ expressionIds.map(e => FormComponentId(e.id)).toSet

    def dateExprInvalidRefs(dateExpr: DateExpr*): Seq[FormComponentId] =
      dateExpr.flatMap(
        _.maybeFormCtx.flatMap(fCtx => if (!allFcIds(fCtx.formComponentId)) List(fCtx.formComponentId) else List())
      )

    def invalid(path: TemplatePath, formComponentIds: FormComponentId*): Invalid =
      Invalid(s"${path.path}: ${formComponentIds.mkString(",")} doesn't exist in the form")

    val exprValidationResults: ValidationResult = allExprs.flatMap(_.referenceInfos).foldMap {
      case ReferenceInfo.FormCtxExpr(path, FormCtx(formComponentId)) if !fcIdsWithExprs(formComponentId) =>
        invalid(path, formComponentId)
      case ReferenceInfo.SumExpr(path, Sum(FormCtx(formComponentId))) if !allFcIds(formComponentId) =>
        invalid(path, formComponentId)
      case ReferenceInfo.CountExpr(path, Count(formComponentId)) if !allFcIds(formComponentId) =>
        invalid(path, formComponentId)
      case ReferenceInfo.PeriodExpr(path, Period(DateCtx(dateExpr1), DateCtx(dateExpr2)))
          if dateExprInvalidRefs(dateExpr1, dateExpr2).nonEmpty =>
        invalid(path, dateExprInvalidRefs(dateExpr1, dateExpr2): _*)
      case ReferenceInfo.PeriodExtExpr(path, PeriodExt(Period(DateCtx(dateExpr1), DateCtx(dateExpr2)), _))
          if dateExprInvalidRefs(dateExpr1, dateExpr2).nonEmpty =>
        invalid(path, dateExprInvalidRefs(dateExpr1, dateExpr2): _*)
      case ReferenceInfo.BetweenExpr(path, Between(DateCtx(dateExpr1), DateCtx(dateExpr2), _))
          if dateExprInvalidRefs(dateExpr1, dateExpr2).nonEmpty =>
        invalid(path, dateExprInvalidRefs(dateExpr1, dateExpr2): _*)
      case ReferenceInfo.SizeExpr(path, Size(formComponentId, _)) if !allFcIds(formComponentId) =>
        invalid(path, formComponentId)
      case ReferenceInfo.LinkCtxExpr(path, LinkCtx(PageLink(pageId))) if !allPageIds.contains(pageId) =>
        Invalid(s"${path.path}: Page id '${pageId.id}' doesn't exist in the form")
      case ReferenceInfo.LookupColumnExpr(path, LookupColumn(formComponentId, _)) if !allFcIds(formComponentId) =>
        invalid(path, formComponentId)
      case ReferenceInfo.IndexOfExpr(path, IndexOf(formComponentId, _)) if !allFcIds(formComponentId) =>
        invalid(path, formComponentId)
      case ReferenceInfo.NumberedListExpr(path, NumberedList(formComponentId))
          if !SectionHelper
            .addToListFormComponents(formTemplate.formKind.allSections)
            .map(_.id)
            .contains(formComponentId) && !allChoiceCheckboxIds.contains(formComponentId) =>
        Invalid(
          s"${path.path}: $formComponentId is not AddToList ID or a checkbox component (choice) ID"
        )
      case ReferenceInfo.IndexExpr(path, Index(formComponentId))
          if !SectionHelper
            .addToListAddAnotherQuestionIds(formTemplate.formKind.allSections)
            .map(_.id)
            .contains(formComponentId) =>
        Invalid(s"${path.path}: $formComponentId is not AddToList addAnotherQuestionId")
      case ReferenceInfo.BulletedListExpr(path, BulletedList(formComponentId))
          if !SectionHelper
            .addToListFormComponents(formTemplate.formKind.allSections)
            .map(_.id)
            .contains(formComponentId) && !allChoiceCheckboxIds
            .contains(formComponentId) && !allRevealingChoiceCheckboxIds.contains(formComponentId) =>
        Invalid(
          s"${path.path}: $formComponentId is not AddToList ID or a checkbox component (choice or Revealing Choice) ID"
        )
      case BulletedListChoicesSelectedExpr(path, BulletedListChoicesSelected(formComponentId, _))
          if !SectionHelper
            .addToListFormComponents(formTemplate.formKind.allSections)
            .map(_.id)
            .contains(formComponentId) && !allChoiceCheckboxIds
            .contains(formComponentId) =>
        Invalid(
          s"${path.path}: $formComponentId is not AddToList ID or a checkbox choice component ID"
        )
      case NumberedListChoicesSelectedExpr(path, NumberedListChoicesSelected(formComponentId, _))
          if !SectionHelper
            .addToListFormComponents(formTemplate.formKind.allSections)
            .map(_.id)
            .contains(formComponentId) && !allChoiceCheckboxIds
            .contains(formComponentId) =>
        Invalid(
          s"${path.path}: $formComponentId is not AddToList ID or a checkbox choice component ID"
        )
      case ReferenceInfo.StringOpsExpr(path, StringOps(FormCtx(formComponentId), _)) if !allFcIds(formComponentId) =>
        invalid(path, formComponentId)
      case ReferenceInfo.ChoicesRevealedFieldExpr(path, ChoicesRevealedField(formComponentId))
          if !SectionHelper
            .addToListFormComponents(formTemplate.formKind.allSections)
            .collect({ case fc @ IsRevealingChoice(_) => fc })
            .find(_.id === formComponentId)
            .exists {
              case IsRevealingChoice(_) => true
              case _                    => false
            } =>
        Invalid(s"${path.path}: $formComponentId is not a Revealing Choice in ATL")
      case ReferenceInfo.ChoicesSelectedExpr(path, ChoicesSelected(formComponentId))
          if !allChoiceIds(formComponentId) =>
        Invalid(s"${path.path}: $formComponentId is not a Choice in the form")
      case ReferenceInfo.CountSelectedChoicesExpr(path, CountSelectedChoices(formComponentId))
          if !allChoiceIds(formComponentId) =>
        Invalid(s"${path.path}: $formComponentId is not a Choice in the form")
      case ReferenceInfo.ChoicesAvailableExpr(path, ChoicesAvailable(formComponentId, _))
          if !allChoiceIds(formComponentId) =>
        Invalid(s"${path.path}: $formComponentId is not a Choice in the form")
      case ReferenceInfo.ChoicesCountExpr(path, ChoicesCount(formComponentId)) if !allChoiceIds(formComponentId) =>
        Invalid(s"${path.path}: $formComponentId is not a Choice in the form")
      case ReferenceInfo.TaskStatusExpr(path, TaskStatus(taskId)) if !allTaskIds(taskId) =>
        Invalid(s"${path.path}: ${taskId.id} is not a Task id in the form")
      case DisplayAsEnteredExpr(path, DisplayAsEntered(formComponentId)) if !allTextAreaIds(formComponentId) =>
        Invalid(s"${path.path}: $formComponentId is not a Multiline Text field (Text Area) id in the form")
      case ReferenceInfo.LookupOpsExpr(path, LookupOps(FormCtx(formComponentId), _)) if !allFcIds(formComponentId) =>
        invalid(path, formComponentId)
      case _ => Valid
    }

    val addToListPageToDisplayAfterRemoveRefs: List[PageId] = formTemplate.formKind.allSections.collect {
      case s: Section.AddToList => s.pageIdToDisplayAfterRemove.toList
    }.flatten

    val pageValidationResults: List[ValidationResult] = addToListPageToDisplayAfterRemoveRefs
      .map(pageId =>
        if (!allPageIds.contains(pageId)) {
          Invalid(s"$pageId: doesn't exist in the form")
        } else {
          Valid
        }
      )

    exprValidationResults.combine(pageValidationResults.combineAll)
  }

  def validateReferencesConstraints(
    formTemplate: FormTemplate,
    allExpressions: List[ExprWithPath]
  ): ValidationResult = {
    val mrc = new MutualReferenceChecker(formTemplate)
    val functionsChecker = new FunctionsChecker(formTemplate, allExpressions)

    Monoid.combineAll(List(mrc.result, functionsChecker.result))
  }

  def validateErrorMessageConstraints(
    formTemplate: FormTemplate,
    allExpressions: List[ExprWithPath]
  ): ValidationResult = {
    val invalidResults: List[ValidationResult] = allExpressions.flatMap(_.referenceInfos) collect {
      case FormCtxExpr(path, FormCtx(fcId))
          if path.subpaths.contains("errorMessage") && !noPIIFcIds(formTemplate).contains(fcId) &&
            !path.subpaths.lastOption.exists(_ === "includeIf") =>
        Invalid(s"""${path.path} contains PII fcId: ${fcId.value}""")
    }
    Monoid.combineAll(invalidResults)
  }

  def validateNoPIITitleConstraints(
    formTemplate: FormTemplate,
    allExpressions: List[ExprWithPath]
  ): ValidationResult = {
    val exprRefs: List[(Expr, FormComponentId)] = allExpressions.flatMap(x =>
      x.referenceInfos.collect {
        case FormCtxExpr(_, FormCtx(fcId)) =>
          (x.expr, fcId)
        case IndexOfExpr(_, IndexOf(fcId, _)) =>
          (x.expr, fcId)
      }
    )
    val idsWithNoPII = noPIIFcIds(formTemplate)
    type PIIErrorMessage = String
    type PIIFormComponentId = String
    def check(
      title: SmartString,
      noPIITitle: Option[SmartString],
      piiErrorMessage: PIIErrorMessage
    ): List[(PIIErrorMessage, PIIFormComponentId)] = {
      val exprs = noPIITitle.fold(title.internals.flatMap(_.interpolations))(_ => List.empty)
      val fcIds = exprRefs.collect { case (e, fcId) if exprs.contains(e) => fcId }
      fcIds.filterNot(idsWithNoPII.contains).map(_.value).distinct.map(x => (piiErrorMessage, x))
    }

    val titleMessage = "can only be used in title if it does not contain PII or noPIITitle is defined"
    val updateTitleMessage = "can only be used in updateTitle if it does not contain PII or noPIIUpdateTitle is defined"
    formTemplate.formKind.allSections
      .flatMap(
        _.fold(n => check(n.page.title, n.page.noPIITitle, titleMessage))(r =>
          check(r.title(), r.page.noPIITitle, titleMessage)
        )(a =>
          check(a.title, a.noPIITitle, titleMessage)
            ++ a.defaultPage.toList.flatMap(p => check(p.title, p.noPIITitle, titleMessage))
            ++ a.pages.toList.flatMap(p => check(p.title, p.noPIITitle, titleMessage))
            ++ a.cyaPage.toList.flatMap(c => c.title.toList.flatMap(t => check(t, c.noPIITitle, titleMessage)))
            ++ a.cyaPage.toList.flatMap(c => check(c.updateTitle, c.noPIIUpdateTitle, updateTitleMessage))
        )
      )
      .headOption
      .map { case (piiErrorMessage, piiFcId) =>
        Invalid(s"Field id [$piiFcId] $piiErrorMessage")
      }
      .getOrElse(Valid)
  }

  private[formtemplate] def noPIIFcIds(formTemplate: FormTemplate): List[FormComponentId] =
    (formTemplate.destinations.allFormComponents ++
      SectionHelper.allSectionsFormComponents(formTemplate.formKind.allSections))
      .filter(_.notPII)
      .map(f => f.id)

  def validateAddressReferencesConstraints(
    formTemplate: FormTemplate,
    allExpressions: List[ExprWithPath]
  ): ValidationResult = {
    val addressLensChecker = new AddressLensChecker(formTemplate, allExpressions)
    addressLensChecker.result
  }

  private def mkFcIdToComponentType(formTemplate: FormTemplate): Map[FormComponentId, ComponentType] =
    (formTemplate.destinations.allFormComponents ++ SectionHelper
      .pages(formTemplate.formKind.allSections)
      .flatMap(_.allFormComponents))
      .map(f => (f.id, f.`type`))
      .toMap

  private def validateFormComponentTypeDate(
    path: TemplatePath,
    formComponentId: FormComponentId,
    functionName: String,
    fcIdToComponentType: Map[FormComponentId, ComponentType]
  ) =
    fcIdToComponentType
      .get(formComponentId)
      .fold[ValidationResult](Invalid(s"${path.path}: Form component $formComponentId is invalid")) { componentType =>
        (componentType.cast[Date], componentType.cast[CalendarDate.type]) match {
          case (Some(_), _) | (_, Some(_)) => Valid
          case _ =>
            Invalid(
              s"${path.path}: Form component '$formComponentId' used in $functionName function should be date type"
            )
        }
      }

  def validateDateFunctionReferenceConstraints(
    formTemplate: FormTemplate,
    allExpressions: List[ExprWithPath]
  ): ValidationResult = {

    val fcIdToComponentType: Map[FormComponentId, ComponentType] = mkFcIdToComponentType(formTemplate)

    def validateExpr(formCtx: FormCtx, path: TemplatePath, functionName: String): ValidationResult =
      validateFormComponentTypeDate(path, formCtx.formComponentId, functionName, fcIdToComponentType)

    val validations = allExpressions.flatMap(_.referenceInfos).collect {
      case DateFunctionExpr(path, DateProjection.Day(expr)) =>
        expr.referenceInfos.collect { case FormCtxExpr(path, formCtx) =>
          validateExpr(formCtx, path, "day")
        }
      case DateFunctionExpr(path, DateProjection.Month(expr)) =>
        expr.referenceInfos.collect { case FormCtxExpr(path, formCtx) =>
          validateExpr(formCtx, path, "month")
        }
      case DateFunctionExpr(path, DateProjection.Year(expr)) =>
        expr.referenceInfos.collect { case FormCtxExpr(path, formCtx) =>
          validateExpr(formCtx, path, "year")
        }
      case DateFunctionExpr(path, DateProjection.TaxYear(expr)) =>
        expr.referenceInfos.collect { case FormCtxExpr(path, formCtx) =>
          validateExpr(formCtx, path, "taxYear")
        }
    }
    Monoid.combineAll(validations.flatten)
  }

  def validateDateConstructExpressions(
    allExpressions: List[ExprWithPath]
  ): ValidationResult = {
    val validations = allExpressions.collect { case ExprWithPath(path, DateCtx(DateConstructExpr(dExpr, _))) =>
      dExpr match {
        case DateValueExpr(ExactDateExprValue(_, m, d)) if (m === 2) && (d > 28) =>
          Invalid(s"${path.path}: yearToDate can not be used with February 29th")
        case _ => Valid
      }
    }

    Monoid.combineAll(validations)
  }

  def validatePeriodFunReferenceConstraints(
    formTemplate: FormTemplate,
    allExpressions: List[ExprWithPath]
  ): ValidationResult = {

    val fcIdToComponentType: Map[FormComponentId, ComponentType] = mkFcIdToComponentType(formTemplate)

    def validateExpr(expr: Expr, path: TemplatePath): ValidationResult =
      expr
        .cast[DateCtx]
        .map(
          _.value.maybeFormCtx
            .foldLeft[ValidationResult](Valid)((v, formCtx) =>
              v.combine(validateFormComponentTypeDate(path, formCtx.formComponentId, "period", fcIdToComponentType))
            )
        )
        .getOrElse(Invalid(s"${path.path}: Expression $expr used in period function should be a date expression"))

    val validations = allExpressions.flatMap(_.referenceInfos).collect {
      case PeriodExpr(path, Period(expr1, expr2)) => List(validateExpr(expr1, path), validateExpr(expr2, path))
      case PeriodExtExpr(path, PeriodExt(Period(expr1, expr2), _)) =>
        List(validateExpr(expr1, path), validateExpr(expr2, path))
    }
    Monoid.combineAll(validations.flatten)
  }

  def validateBetweenFunReferenceConstraints(
    formTemplate: FormTemplate,
    allExpressions: List[ExprWithPath]
  ): ValidationResult = {

    val fcIdToComponentType: Map[FormComponentId, ComponentType] = mkFcIdToComponentType(formTemplate)

    def validateExpr(expr: Expr, path: TemplatePath): ValidationResult =
      expr
        .cast[DateCtx]
        .map(
          _.value.maybeFormCtx
            .foldLeft[ValidationResult](Valid)((v, formCtx) =>
              v.combine(
                validateFormComponentTypeDate(
                  path,
                  formCtx.formComponentId,
                  "daysBetween/weeksBetween",
                  fcIdToComponentType
                )
              )
            )
        )
        .getOrElse(
          Invalid(
            s"${path.path}: Expression $expr used in daysBetween/weeksBetween function should be a date expression"
          )
        )

    val validations = allExpressions.flatMap(_.referenceInfos).collect {
      case BetweenExpr(path, Between(expr1, expr2, _)) => List(validateExpr(expr1, path), validateExpr(expr2, path))
      case SumExpr(path, Sum(Between(dateCtx1, dateCtx2, _))) =>
        List(validateExpr(dateCtx1, path), validateExpr(dateCtx2, path))
    }
    Monoid.combineAll(validations.flatten)
  }

  def allFormComponents(sectionsList: List[Page]): List[FormComponent] = {
    val nestedFields = sectionsList
      .flatMap { page =>
        page.fields.collect {
          case IsGroup(group)                     => group.fields
          case IsRevealingChoice(revealingChoice) => revealingChoice.options.toList.flatMap(_.revealingFields)
        }.flatten
      }
    nestedFields ++ sectionsList.flatMap(_.fields)
  }

  def validateChoice(
    sectionsList: List[Page],
    choiceChecker: Choice => Boolean,
    errorMessage: String,
    detailedError: Choice => String = _ => ""
  ): ValidationResult = {
    val choiceFieldIds = allFormComponents(sectionsList)
      .map(fv => (fv.id, fv.`type`))
      .collect {
        case (fId, choice: Choice) if choiceChecker(choice) =>
          fId
      }

    def detailedReport(formComponentId: FormComponentId): String =
      allFormComponents(sectionsList)
        .map(fv => (fv.id, fv.`type`))
        .collectFirst {
          case (fId, choice: Choice) if formComponentId === fId =>
            val de = detailedError(choice)
            if (de.isEmpty) de else " " + de
        }
        .getOrElse("")

    choiceFieldIds.isEmpty.validationResult(
      errorMessage + ": " + choiceFieldIds.headOption
        .map(fcId => fcId.value + "." + detailedReport(fcId))
        .getOrElse("")
    )
  }

  def validateRevealingChoice(
    sectionsList: List[Page],
    revealingChoiceChecker: RevealingChoice => Boolean,
    errorMessage: String
  ): ValidationResult = {
    val revealingChoiceFieldIds = allFormComponents(sectionsList)
      .map(fv => (fv.id, fv.`type`))
      .collect {
        case (fId, rc: RevealingChoice) if revealingChoiceChecker(rc) =>
          fId
      }

    revealingChoiceFieldIds.isEmpty.validationResult(errorMessage + ": " + revealingChoiceFieldIds.mkString(","))
  }

  def validateChoiceHelpText(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean =
      choice.optionHelpText.fold(false)(helpTexts => choice.options.size != helpTexts.size)

    validateChoice(sectionsList, check, "Choice components doesn't have equal number of choices and help texts")
  }

  def validateChoiceHints(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean =
      choice.hints.fold(false)(hints => choice.options.size != hints.size)

    validateChoice(sectionsList, check, "Choice components doesn't have equal number of choices and hints")
  }

  def validateChoiceDividerPositionValue(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean = choice.dividerPosition.exists {
      case DividerPosition.Number(i) => false
      case DividerPosition.Value(d) =>
        choice.options.collectFirst {
          case OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(value), _, _) if value === d => true
        }.isEmpty
    }

    validateChoice(sectionsList, check, "dividerPosition value should be one of the non dynamic choice options")
  }

  def validateChoiceDividerPositionLowerBound(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean = choice.dividerPosition.exists {
      case DividerPosition.Number(i) => i <= 0
      case DividerPosition.Value(_)  => false
    }

    validateChoice(sectionsList, check, "dividerPosition should be greater than 0 value")
  }

  def validateChoiceDividerPositionUpperBound(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean = choice.dividerPosition.exists {
      case DividerPosition.Number(i) => i >= choice.options.size
      case DividerPosition.Value(_)  => false
    }

    def details(choice: Choice): String = {
      val choices = choice.options.map {
        case OptionData.ValueBased(ss, _, _, _, _, _, _) => ss.defaultRawValue(LangADT.En)
        case OptionData.IndexBased(ss, _, _, _, _)       => ss.defaultRawValue(LangADT.En)
      }

      "Got choices:" + choices + ", dividerPosition: " + choice.dividerPosition

    }

    validateChoice(sectionsList, check, "dividerPosition should be less than the number of choices", details)
  }

  def validateChoiceNoneChoiceValue(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean = {
      val values = choice.options.collect {
        case OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(v), _, _) =>
          v
      }
      choice.noneChoice.fold(false) {
        case NoneChoice.IndexBased(index) => values.nonEmpty
        case NoneChoice.ValueBased(value) => !values.contains(value)
      }
    }

    def details(choice: Choice): String = {
      val values = choice.options.collect {
        case OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(v), _, _) =>
          v
      }

      val got = choice.noneChoice.fold("noneChoice was not provided") {
        case NoneChoice.IndexBased(index) => index.toString
        case NoneChoice.ValueBased(value) => value
      }

      "Got " + got + ", expected one of " + values

    }

    validateChoice(
      sectionsList,
      check,
      s"'noneChoice' should match one of choices 'value'",
      details
    )
  }

  def validateChoiceNoneChoiceLowerBound(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean = choice.noneChoice.fold(false) {
      case NoneChoice.IndexBased(index) => index <= 0
      case NoneChoice.ValueBased(value) => false
    }

    validateChoice(sectionsList, check, "noneChoice should be greater than 0")
  }

  def validateChoiceNoneChoiceUpperBound(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean = choice.noneChoice.fold(false) {
      case NoneChoice.IndexBased(index) => index > choice.options.size
      case NoneChoice.ValueBased(value) => false
    }

    validateChoice(sectionsList, check, "noneChoice should be less than the number of choices")
  }

  def validateChoiceNoneChoiceMultivalueOnly(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean = choice.noneChoice.isDefined && (choice.`type` != Checkbox)

    validateChoice(sectionsList, check, "noneChoice can only be used together with multivalue")
  }

  def validateChoiceNoneChoiceAndError(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean =
      choice.`type` === Checkbox && choice.noneChoice.isDefined =!= choice.noneChoiceError.isDefined

    def details(choice: Choice): String = {
      val c = choice.noneChoice.map(_.fold(_.index.toString())(_.value))
      val ce = choice.noneChoiceError.map(_.m.values.mkString(","))
      s"Got noneChoice: $c, noneChoiceError: $ce"
    }

    validateChoice(sectionsList, check, "noneChoice and noneChoiceError should be defined", details)
  }

  def validateChoiceOptions(sectionsList: List[Page]): ValidationResult = {
    def checkUniformness(choice: Choice): Boolean = {
      val dynamicOptionExists: Boolean = choice.options.exists {
        case o: OptionData.ValueBased => o.dynamic.isDefined
        case o: OptionData.IndexBased => o.dynamic.isDefined
        case _                        => false
      }
      //If one of the option values is dynamic, uniformness validation is not required.
      if (dynamicOptionExists) false
      else {
        val indexed = choice.options.collect { case o: OptionData.IndexBased =>
          o
        }
        indexed.size =!= 0 && indexed.size =!= choice.options.size
      }
    }

    def checkUnique(choice: Choice): Boolean = {
      val values = choice.options.collect { case OptionData.ValueBased(_, _, _, _, value, _, _) =>
        value
      }
      values.size =!= values.distinct.size
    }

    def checkNoBannedStringInValue(choice: Choice): Boolean = {
      val values = getStringBasedOptionDataValues(choice)
      values.size =!= 0 && !"""\w+""".r.pattern.matcher(values.mkString).matches
    }

    def checkNotOnlySpacesOrBlank(choice: Choice): Boolean =
      getStringBasedOptionDataValues(choice).exists(_.trim.isEmpty)

    def checkHideChoicesSelected(choice: Choice): Boolean =
      if (choice.hideChoicesSelected) {
        choice.options.exists {
          case _: OptionData.IndexBased => true
          case _: OptionData.ValueBased => false
        }
      } else false

    def checkHideChoicesSelectedNonDynamicOptions(choice: Choice): Boolean =
      if (choice.hideChoicesSelected) {
        areChoiceOptionsDataRetrievedBased(Option(choice))
      } else false

    def getStringBasedOptionDataValues(choice: Choice): List[String] =
      choice.options.collect { case OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(value), _, _) =>
        value
      }

    List(
      validateChoice(
        sectionsList,
        checkUniformness,
        "Choice component has some options with 'value' and some options without 'value'"
      ),
      validateChoice(
        sectionsList,
        checkUnique,
        "Choice component options 'value's needs to be unique"
      ),
      validateChoice(
        sectionsList,
        checkNoBannedStringInValue,
        "Choice component options non-expr 'value' must only contain letters, numbers and underscores"
      ),
      validateChoice(
        sectionsList,
        checkNotOnlySpacesOrBlank,
        "Choice component options cannot be empty or include only spaces"
      ),
      validateChoice(
        sectionsList,
        checkHideChoicesSelected,
        "'hideChoicesSelected: true' can be used only on choices with values"
      ),
      validateChoice(
        sectionsList,
        checkHideChoicesSelectedNonDynamicOptions,
        "'hideChoicesSelected: true' for ATL cannot be used with dynamic choice options from a data retrieve"
      )
    ).combineAll
  }

  def validateRevealingChoiceOptions(sectionsList: List[Page]): ValidationResult = {
    def checkUniformness(revealingChoice: RevealingChoice): Boolean = {
      val choices = revealingChoice.options.map(_.choice)
      val indexed = choices.collect { case o: OptionData.IndexBased =>
        o
      }
      indexed.size =!= 0 && indexed.size =!= choices.size
    }

    def checkUnique(revealingChoice: RevealingChoice): Boolean = {
      val choices = revealingChoice.options.map(_.choice)
      val values = choices.collect { case OptionData.ValueBased(_, _, _, _, value, _, _) =>
        value
      }
      values.size =!= values.distinct.size
    }

    List(
      validateRevealingChoice(
        sectionsList,
        checkUniformness,
        "Revealing choice component has some choices with 'value' and some choices without 'value'"
      ),
      validateRevealingChoice(
        sectionsList,
        checkUnique,
        "Revealing choice component choices 'value's needs to be unique"
      )
    ).combineAll
  }

  def validatePageRedirects(sectionsList: List[Page]): ValidationResult = {
    val redirectCtxs: List[RedirectCtx] = sectionsList.flatMap(_.redirects.toList).flatMap(_.toList)
    redirectCtxs
      .flatMap(redirectCtx => redirectCtx.redirectUrl.internals)
      .map { ss =>
        val rawValue = ss.rawValue(LangADT.En).trim
        val singleExpressionOnly = rawValue === "{0}"
        val validationResult: ValidationResult = ss.interpolations match {
          case Nil                                       => Valid // Free text is ok
          case LinkCtx(_) :: Nil if singleExpressionOnly => Valid
          case _ =>
            Invalid(s"redirectUrl: $rawValue is not valid. Only 'text only' or single link expression is allowed")
        }
        validationResult
      }
      .combineAll
  }

  def validateLabel(sectionsList: List[Page]): ValidationResult = {
    def validateChoiceSingleChoice(sectionsList: List[Page]): ValidationResult = {
      def check(choice: Choice): Boolean = choice.options.length <= 1

      validateChoice(sectionsList, check, "more than one options in the choice component")
    }
    def isEmptyLabel(label: SmartString): Boolean =
      label.internals
        .forall(internalSmartString => internalSmartString.rawValue(LangADT.En).trim === "")

    val validationResults = sectionsList.flatMap { page =>
      (page.allFormComponents ++ page.fields).collect {
        case fc @ IsAddress(_) if isEmptyLabel(fc.label) =>
          Invalid(s"address component ${fc.id} should have a non-blank label")
        case fc @ IsOverseasAddress(_) if isEmptyLabel(fc.label) =>
          Invalid(s"overseas address component ${fc.id} should have a non-blank label")
        case fc @ IsRevealingChoice(_) if isEmptyLabel(fc.label) =>
          Invalid(s"revealing choice component ${fc.id} should have a non-blank label")
        case fc @ IsDate(_) if isEmptyLabel(fc.label) =>
          Invalid(s"date component ${fc.id} should have a non-blank label")
        case fc @ IsChoice(_) if isEmptyLabel(fc.label) && validateChoiceSingleChoice(sectionsList).isValid =>
          Invalid(s"choice component ${fc.id} should have a non-blank label")
        case fc @ IsText(_) if isEmptyLabel(fc.label) && !fc.onlyShowOnSummary =>
          Invalid(s"text component ${fc.id} should have a non-blank label, unless submitMode is summaryinfoonly")
        case fc @ IsTextArea(_) if isEmptyLabel(fc.label) && !fc.onlyShowOnSummary =>
          Invalid(s"text area component ${fc.id} should have a non-blank label, unless submitMode is summaryinfoonly")
        case fc @ IsFileUpload(_) if isEmptyLabel(fc.label) =>
          Invalid(s"fileUpload component ${fc.id} should have a non-blank label")
        case fc @ IsInformationMessage(im) if isEmptyLabel(fc.label) && im.summaryValue.isDefined =>
          Invalid(s"info component ${fc.id} should have a non-blank label if summaryValue is specified")
      }
    }

    Monoid[ValidationResult].combineAll(validationResults)
  }

  def validatePostcodeLookup(sectionsList: List[Page]): ValidationResult = {
    val pagesWithPostcodeLookups: List[(FormComponentId, Page)] = sectionsList.flatMap { page =>
      page.fields.collect { case fc @ IsPostcodeLookup(_) =>
        fc.id -> page
      }
    }
    val noOtherComponentsAllowed: ValidationResult = pagesWithPostcodeLookups.foldMap { case (postcodeId, page) =>
      val filteredFields = page.fields.filter {
        case IsPostcodeLookup(_) | IsInformationMessage(_) => true
        case _                                             => false
      }
      if (filteredFields.size === page.fields.size) {
        Valid
      } else
        Invalid(
          s"Postcode lookup '$postcodeId' cannot be on the page with other components (exception are info components)"
        )
    }
    val noPostcodeLookupInReveliangChoiceOrGroup: ValidationResult = sectionsList
      .flatMap { page =>
        page.fields.collect {
          case IsGroup(group)                     => group.fields
          case IsRevealingChoice(revealingChoice) => revealingChoice.options.toList.flatMap(_.revealingFields)
        }.flatten
      }
      .collectFirst { case fc @ IsPostcodeLookup(_) =>
        fc
      }
      .fold[ValidationResult](Valid)(fc =>
        Invalid(s"Postcode lookup '${fc.id.value}' cannot be inside Revealing choice or Group")
      )

    val postcodeLookups: List[List[FormComponentId]] = sectionsList.map { page =>
      page.allFormComponents.collect { case fc @ IsPostcodeLookup(_) =>
        fc.id
      }
    }
    val singlePostcodeLookupOnly: ValidationResult = postcodeLookups.foldMap { lookups =>
      if (lookups.size > 1) {
        val invalid = lookups.mkString(", ")
        Invalid(
          s"Only one Postcode lookup on a page is allowed. Some page has ${lookups.size} postcode lookups: $invalid"
        )
      } else Valid
    }

    val labelOrShortNameNotEmpty: ValidationResult = sectionsList.flatMap { page =>
      page.allFormComponents.collect { case fc @ IsPostcodeLookup(_) =>
        if (fc.label.allNonEmpty || fc.shortName.map(_.allNonEmpty).getOrElse(false)) Valid
        else
          Invalid(
            s"A label (or shortName) is required for postcodeLookup '${fc.id.value}' to use on summarySection."
          )
      }
    }.combineAll

    Monoid.combineAll(
      List(
        singlePostcodeLookupOnly,
        noPostcodeLookupInReveliangChoiceOrGroup,
        noOtherComponentsAllowed,
        labelOrShortNameNotEmpty
      )
    )

  }

  def validateChoiceSize(sectionsList: List[Page], allExpressions: List[ExprWithPath]): ValidationResult = {
    val fcs: List[(FormComponentId, SizeRefType)] = allExpressions
      .flatMap(_.referenceInfos)
      .collect { case SizeExpr(_, Size(formComponentId, i)) =>
        formComponentId -> i
      }

    def validateOptions(
      options: List[OptionData]
    ): List[ValidationResult] =
      options.collect { case OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(value), _, _) =>
        value match {
          case SizeRefType.regex(_*) => Valid
          case _ =>
            Invalid(
              s"Wrong value '$value'. Only ${SizeRefType.regex.regex} characters are allowed"
            )
        }
      }

    def validateSize(
      formComponentId: FormComponentId,
      options: NonEmptyList[OptionData],
      index: SizeRefType
    ): ValidationResult = {
      val availableOptions = options.toList.collect {
        case OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(value), _, _) =>
          value
      }
      val availableOptionsStr = availableOptions.mkString(", ")
      val size = options.size
      val maxIndex = size - 1
      index match {
        case SizeRefType.IndexBased(index) =>
          val exprStr = s"$formComponentId.$index.size"
          if (availableOptions.nonEmpty) {
            Invalid(
              s"Expression '$exprStr' can't use numeric index: $index. $formComponentId has these values: $availableOptionsStr"
            )
          } else if (index > maxIndex)
            Invalid(
              s"Expression '$exprStr' has wrong index $index. $formComponentId has only $size elements. Use index from 0 to $maxIndex"
            )
          else Valid
        case SizeRefType.ValueBased(index) =>
          val exprStr = s"$formComponentId.$index.size"
          if (availableOptions.contains(index)) {
            Valid
          } else if (availableOptions.nonEmpty)
            Invalid(
              s"Expression '$exprStr' has wrong value $index. $formComponentId has these values: $availableOptionsStr"
            )
          else {
            Invalid(
              s"Expression '$exprStr' can't use value index: $index. Use numeric index from 0 to $maxIndex"
            )
          }
      }
    }

    def validateSizeExpr(
      sectionsList: List[Page],
      formComponentId: FormComponentId,
      index: SizeRefType
    ): List[ValidationResult] =
      sectionsList
        .flatMap(_.fields)
        .filter(_.id === formComponentId)
        .map(fv => fv.`type`)
        .collect {
          case (choice: Choice) =>
            validateSize(formComponentId, choice.options, index)
          case (revealingChoice: RevealingChoice) =>
            validateSize(formComponentId, revealingChoice.options.map(_.choice), index)
          case _ =>
            Invalid(
              s"Form component '$formComponentId' used in '$formComponentId.$index.size' expression should be choice or revealing choice type"
            )
        }

    def allChoiceData(
      sectionsList: List[Page]
    ): List[OptionData] =
      sectionsList
        .flatMap(_.fields)
        .map(fv => fv.`type`)
        .collect {
          case choice: Choice                   => choice.options.toList
          case revealingChoice: RevealingChoice => revealingChoice.options.map(_.choice).toList
        }
        .flatten

    val optionDataValidations = validateOptions(allChoiceData(sectionsList))
    val sizeExprValidations = fcs.flatMap { case (fc, index) =>
      validateSizeExpr(sectionsList, fc, index)
    }

    Monoid.combineAll(optionDataValidations ++ sizeExprValidations)
  }

  val userContextComponentType: List[FormComponent] => List[FormComponent] =
    enrolledIdentifierComponents =>
      enrolledIdentifierComponents.collect { case expr @ HasExpr(SingleExpr(UserCtx(UserField.EnrolledIdentifier))) =>
        expr
      }

  def validateEnrolmentIdentifier(formTemplate: FormTemplate): ValidationResult =
    if (userContextComponentType(formTemplate.expandedFormComponentsInMainSections).nonEmpty) {
      formTemplate.authConfig match {
        case HmrcEnrolmentModule(_) | HmrcAgentWithEnrolmentModule(_, _) => Valid
        case _                                                           => Invalid("You used '${user.enrolledIdentifier}' but you didn't provide 'serviceId'.")
      }
    } else Valid

  def validateEnrolmentSection(formTemplate: FormTemplate): ValidationResult =
    formTemplate.authConfig match {
      case HasEnrolmentSection(_, enrolmentSection, _) =>
        val fcIds = enrolmentSection.fields.map(_.id).toSet
        val ctxs = enrolmentSection.identifiers.map(_.value.formComponentId).toList.toSet ++ enrolmentSection.verifiers
          .map(_.value.formComponentId)
          .toSet
        ctxs
          .subsetOf(fcIds)
          .validationResult(
            s"Following identifiers and/or verifiers don't have corresponding field entry: " + ctxs
              .diff(fcIds)
              .mkString(", ")
          )
      case _ => Valid
    }

  def validateRegimeId(formTemplate: FormTemplate): ValidationResult = {
    def regimeIdCheck(regimeId: RegimeId): ValidationResult =
      (regimeId.value.length >= 2 && regimeId.value.length <= 8)
        .validationResult("Regime id must be between 2 and 8 characters long")

    formTemplate.authConfig match {
      case HmrcEnrolmentModule(EnrolmentAuth(_, DoCheck(_, _, RegimeIdCheck(regimeId)), _)) => regimeIdCheck(regimeId)
      case HmrcAgentWithEnrolmentModule(_, EnrolmentAuth(_, DoCheck(_, _, RegimeIdCheck(regimeId)), _)) =>
        regimeIdCheck(regimeId)
      case _ => Valid
    }
  }

  def validateDependencyGraph(formTemplate: FormTemplate): ValidationResult = {
    val graph: Graph[FormComponentId, DiEdge] = toGraph(formTemplate)
    constructDependencyGraph(graph)
      .bimap(const(Invalid(s"Graph contains cycle ${graph.findCycle}")), const(Valid))
      .merge
  }

  def validateTypeAheadChoices(sectionsList: List[Page]): ValidationResult = {
    def checkTypeAheadConstraints(choice: Choice): Boolean =
      choice.`type` match {
        case TypeAhead =>
          val hasIndexBasedOptions = choice.options.exists {
            case _: OptionData.IndexBased => true
            case _: OptionData.ValueBased => false
          }

          val hasDynamicOptions = choice.options.exists {
            case OptionData.ValueBased(_, _, _, Some(_), _, _, _) => true
            case _                                                => false
          }

          hasIndexBasedOptions || hasDynamicOptions
        case _ => false
      }

    def detailedError(choice: Choice): String =
      choice.`type` match {
        case TypeAhead =>
          val errors = scala.collection.mutable.ListBuffer[String]()

          val hasIndexBased = choice.options.exists(_.isInstanceOf[OptionData.IndexBased])
          if (hasIndexBased) {
            errors += "TypeAhead choices must use ValueBased options only"
          }

          val hasDynamic = choice.options.exists {
            case OptionData.ValueBased(_, _, _, Some(_), _, _, _) => true
            case _                                                => false
          }
          if (hasDynamic) {
            errors += "TypeAhead choices cannot use dynamic options"
          }

          errors.mkString("; ")
        case _ => ""
      }

    validateChoice(
      sectionsList,
      checkTypeAheadConstraints,
      "TypeAhead choice validation failed",
      detailedError
    )
  }

  def validateTypeAheadKeyWordUsage(sectionsList: List[Page]): ValidationResult = {
    def checkKeyWordUsage(choice: Choice): Boolean =
      choice.options.exists {
        case OptionData.ValueBased(_, _, _, _, _, _, Some(_)) if choice.`type` != TypeAhead => true
        case _                                                                              => false
      }

    def detailedError(choice: Choice): String = {
      val invalidOptions = choice.options.collect {
        case OptionData.ValueBased(label, _, _, _, _, _, Some(keyWord)) if choice.`type` != TypeAhead =>
          s"'${label.defaultRawValue(LangADT.En)}' (keyWord: ${keyWord.defaultRawValue(LangADT.En)})"
      }
      if (invalidOptions.nonEmpty) {
        s"keyWord property can only be used with TypeAhead choices. Found in options: ${invalidOptions.mkString(", ")}"
      } else {
        ""
      }
    }

    validateChoice(
      sectionsList,
      checkKeyWordUsage,
      "keyWord property can only be used with TypeAhead choices",
      detailedError
    )
  }

  def validate(componentTypes: List[ComponentType], formTemplate: FormTemplate): ValidationResult = {
    val results = componentTypes.map(validate(_, formTemplate))
    Monoid[ValidationResult].combineAll(results)
  }

  @nowarn
  def validate(componentType: ComponentType, formTemplate: FormTemplate): ValidationResult = componentType match {
    case HasExpr(SingleExpr(expr))               => validate(expr, formTemplate.formKind.allSections)
    case HasExpr(MultipleExpr(fields))           => Valid
    case Date(_, _, _)                           => Valid
    case CalendarDate                            => Valid
    case TaxPeriodDate                           => Valid
    case Address(_, _, _, Some(expr))            => validateAddressValue(expr, formTemplate)
    case Address(_, _, _, _)                     => Valid
    case Choice(_, _, _, _, _, _, _, _, _, _, _) => Valid
    case RevealingChoice(revealingChoiceElements, _) =>
      validate(revealingChoiceElements.toList.flatMap(_.revealingFields.map(_.`type`)), formTemplate)
    case HmrcTaxPeriod(_, _, _)                     => Valid
    case Group(fvs, _, _, _, _)                     => validate(fvs.map(_.`type`), formTemplate)
    case FileUpload(_, _)                           => Valid
    case mf @ MultiFileUpload(_, _, _, _, _, _, _)  => validateMultiFileUpload(mf)
    case InformationMessage(_, _, _)                => validateInformationMessages(formTemplate)
    case Time(_, _)                                 => Valid
    case OverseasAddress(_, _, _, Some(expr), _, _) => validateOverseasAddressValue(expr, formTemplate)
    case OverseasAddress(_, _, _, _, _, _)          => Valid
    case PostcodeLookup(_, _, _)                    => Valid
    case MiniSummaryList(ls, _, _)                  => validateMiniSummaryList(ls, formTemplate)
    case t: TableComp                               => TableCompValidator.validateTableComp(t)
    case Button(_, _, _, _)                         => Valid
  }

  private def pdfLinkNotAllowed(fc: FormComponent) =
    s"Field '${fc.id}' is a PdfLink info field. These are only valid in the form's acknowledgement section."

  private def validateInformationMessages(formTemplate: FormTemplate): ValidationResult = {
    val pages = SectionHelper.pages(formTemplate.formKind.allSections)
    pages
      .flatMap(_.fields.map {
        case fc @ IsInformationMessage(InformationMessage(PdfLink, _, _)) => Invalid(pdfLinkNotAllowed(fc))
        case _                                                            => Valid
      })
      .combineAll
  }

  private def validateMultiFileUpload(upload: MultiFileUpload) =
    (for {
      min <- upload.minFiles.collect { case Constant(v) => v.toInt }
      max <- upload.maxFiles.collect { case Constant(v) => v.toInt }
      if min > max
    } yield Invalid(s"minFiles ($min) cannot be greater than maxFiles ($max)"))
      .getOrElse(Valid)

  def validateAddressValue(expr: Expr, formTemplate: FormTemplate): ValidationResult = {
    val sections = formTemplate.formKind.allSections
    val addressIds = SectionHelper.pages(sections).flatMap(page => page.allFormComponents ++ page.fields).collect {
      case fc @ IsAddress(_) => fc.id
    }
    expr match {
      case AuthCtx(AuthInfo.ItmpAddress)                            => Valid
      case DataRetrieveCtx(_, Attribute("registeredOfficeAddress")) => Valid
      case DataRetrieveCtx(_, Attribute("agencyAddress"))           => Valid
      case FormCtx(fcId) if addressIds.contains(fcId)               => Valid
      case _                                                        => Invalid("address value expression should contain either address component id or itmpAddress")
    }
  }

  def validateOverseasAddressValue(expr: Expr, formTemplate: FormTemplate): ValidationResult = {
    val sections = formTemplate.formKind.allSections
    val addressIds = SectionHelper.pages(sections).flatMap(page => page.allFormComponents ++ page.fields).collect {
      case fc @ IsOverseasAddress(_) => fc.id
    }
    expr match {
      case AuthCtx(AuthInfo.ItmpAddress)                  => Valid
      case DataRetrieveCtx(_, Attribute("agencyAddress")) => Valid
      case FormCtx(fcId) if addressIds.contains(fcId)     => Valid
      case _                                              => Invalid("address value expression should contain either overseas address component id or itmpAddress")
    }
  }

  def validateMiniSummaryList(rows: List[MiniSummaryRow], formTemplate: FormTemplate): ValidationResult = {
    val atlIds = SectionHelper.addToListIds(formTemplate.formKind.allSections).map(_.id)

    val pageIds: List[PageId] =
      formTemplate.formKind.allSections.flatMap(
        _.fold(_.page.id.toList)(_.page.id.toList)(p =>
          p.defaultPage.flatMap(_.id).toList ++ p.pages.toList.flatMap(_.id)
        )
      ) ++ atlIds.map(fcId => PageId(fcId.value))

    val taskIds: List[TaskId] =
      formTemplate.formKind.fold(classic => List.empty[TaskId])(taskList =>
        taskList.sections.toList.flatMap(_.tasks.toList.flatMap(_.id))
      )

    Monoid.combineAll(rows.map {
      case MiniSummaryRow.ATLRow(atlId, _, _) =>
        if (atlIds.contains(atlId)) Valid else Invalid(s"$atlId is not AddToList Id")
      case MiniSummaryRow.ValueRow(_, _, _, Some(pageId), _) if !pageIds.contains(pageId) =>
        Invalid(s"${pageId.id} is not a Page Id")
      case MiniSummaryRow.ValueRow(_, _, _, _, Some(taskId)) if !taskIds.contains(taskId) =>
        Invalid(s"${taskId.id} is not a Task Id")
      case MiniSummaryRow.ValueRow(_, _, _, Some(pageId), Some(taskId)) =>
        Invalid(s"Only one of 'pageId: ${pageId.id}', 'taskId: ${taskId.id}' can be defined")
      case MiniSummaryRow.SmartStringRow(_, _, _, Some(pageId), _) if !pageIds.contains(pageId) =>
        Invalid(s"${pageId.id} is not a Page Id")
      case MiniSummaryRow.SmartStringRow(_, _, _, _, Some(taskId)) if !taskIds.contains(taskId) =>
        Invalid(s"${taskId.id} is not a Task Id")
      case MiniSummaryRow.SmartStringRow(_, _, _, Some(pageId), Some(taskId)) =>
        Invalid(s"Only one of 'pageId: ${pageId.id}', 'taskId: ${taskId.id}' can be defined")
      case _ => Valid
    })
  }

  def validateForwardReference(sections: List[Section]): ValidationResult = {
    val indexLookup: Map[FormComponentId, Int] = indexedFieldIds(sections).toMap
    val addToListIds: List[FormComponentId] = SectionHelper.addToListIds(sections).map(_.id)
    val verifyValidIf = new BooleanExprValidator(indexLookup, BooleanExprWrapperType.ValidIf, addToListIds)(_)
    val verifyIncludeIf = new BooleanExprValidator(indexLookup, BooleanExprWrapperType.IncludeIf, addToListIds)(_)
    val verifyRemoveItemIf = new BooleanExprValidator(indexLookup, BooleanExprWrapperType.RemoveItemIf, addToListIds)(_)
    Monoid[ValidationResult]
      .combineAll(
        SectionHelper
          .pages(sections)
          .zipWithIndex
          .flatMap { case (page: Page, idx) =>
            val allValidIfs: List[BooleanExpr] = page.allFormComponents.flatMap(_.allValidIfs).map(_.booleanExpr)
            val verifiedValidIf = allValidIfs.flatMap(verifyValidIf(idx).apply)

            val allComponentIncludeIfs: List[BooleanExpr] =
              page.allFormComponents.flatMap(_.includeIf).map(_.booleanExpr)
            val verifiedComponentIncludeIfs = allComponentIncludeIfs.flatMap(verifyIncludeIf(idx).apply)

            val verifiedIncludeIf = page.includeIf
              .map(_.booleanExpr)
              .map(verifyIncludeIf(idx).apply)
              .getOrElse(List(Valid))

            val verifiedRemoveItemIf = page.removeItemIf
              .map(_.booleanExpr)
              .map(verifyRemoveItemIf(idx).apply)
              .getOrElse(List(Valid))

            val smartStringsToCheck: List[Option[SmartString]] = page.fields.flatMap { field =>
              List(Some(field.label), field.helpText)
            } :+ Some(page.title)

            val samePageReferences = smartStringsToCheck
              .filter(_.isDefined)
              .flatMap(_.get.internals.flatMap(_.interpolations))
              .collect { case FormCtx(fcId) =>
                indexLookup.get(fcId) match {
                  case Some(index) if index === idx =>
                    Invalid(
                      s"Cannot reference a field (${fcId.value}) on the same page"
                    )
                  case _ => Valid
                }
              }

            verifiedValidIf ++ verifiedIncludeIf ++ verifiedComponentIncludeIfs ++ verifiedRemoveItemIf ++ samePageReferences
          }
      )
  }

  def validate(expr: Expr, sections: List[Section]): ValidationResult = {
    val fieldNamesIds: List[FormComponentId] = fieldIds(sections)

    def checkFields(field1: Expr, field2: Expr): ValidationResult = {
      val checkField1 = validate(field1, sections)
      val checkField2 = validate(field2, sections)
      Monoid[ValidationResult].combineAll(List(checkField1, checkField2))
    }

    // TODO verification of invalid references is newly done by LeafExpr logic. Investigate if this can be removed as duplicity.
    expr match {
      case Add(field1, field2)          => checkFields(field1, field2)
      case Subtraction(field1, field2)  => checkFields(field1, field2)
      case Multiply(field1, field2)     => checkFields(field1, field2)
      case Divide(field1, field2)       => checkFields(field1, field2)
      case HideZeroDecimals(field1)     => validate(field1, sections)
      case IfElse(cond, field1, field2) => checkFields(field1, field2) // cond is covered by LeafExpr
      case Else(field1, field2)         => checkFields(field1, field2)
      case Sum(value)                   => validate(value, sections)
      case Count(value)                 => validate(FormCtx(value), sections)
      case Index(value)                 => validate(FormCtx(value), sections)
      case FormCtx(value) =>
        fieldNamesIds
          .contains(value)
          .validationResult(s"Form field '$value' is not defined in form template.")
      case ParamCtx(_)           => Valid
      case AuthCtx(_)            => Valid
      case UserCtx(_)            => Valid
      case Constant(_)           => Valid
      case PeriodValue(_)        => Valid
      case Value                 => Valid
      case FormTemplateCtx(_)    => Valid
      case LinkCtx(_)            => Valid
      case LangCtx               => Valid
      case AddressLens(value, _) => validate(FormCtx(value), sections)
      case DateCtx(value) =>
        val invalidFCIds = value.leafExprs
          .collect { case FormCtx(formComponentId) =>
            formComponentId
          }
          .filter(!fieldNamesIds.contains(_))
          .map(_.value)
        invalidFCIds.isEmpty.validationResult(
          s"Form field(s) '${invalidFCIds.mkString(",")}' not defined in form template."
        )
      case DateFunction(value) => Valid
      case Period(dateCtx1, dateCtx2) =>
        checkFields(dateCtx1, dateCtx2)
      case PeriodExt(periodFun, _) => validate(periodFun, sections)
      case Between(dateCtx1, dateCtx2, _) =>
        checkFields(dateCtx1, dateCtx2)
      case DataRetrieveCtx(_, _)  => Valid
      case DataRetrieveCount(_)   => Valid
      case LookupColumn(value, _) => validate(FormCtx(value), sections)
      case CsvCountryCountCheck(value, _, _) =>
        SectionHelper
          .addToListIds(sections)
          .map(_.id)
          .contains(value)
          .validationResult(s"$value is not AddToList Id")
      case Size(value, _)                    => validate(FormCtx(value), sections)
      case Typed(expr, tpe)                  => validate(expr, sections)
      case IndexOf(formComponentId, _)       => validate(FormCtx(formComponentId), sections)
      case IndexOfDataRetrieveCtx(_, _)      => Valid
      case NumberedList(_)                   => Valid
      case BulletedList(_)                   => Valid
      case NumberedListChoicesSelected(_, _) => Valid
      case BulletedListChoicesSelected(_, _) => Valid
      case StringOps(_, _)                   => Valid
      case Concat(exprs)                     => Monoid.combineAll(exprs.map(e => validate(e, sections)))
      case CountryOfItmpAddress              => Valid
      case ChoicesRevealedField(_)           => Valid
      case ChoicesSelected(_)                => Valid
      case ChoicesAvailable(_, _)            => Valid
      case DisplayAsEntered(_)               => Valid
      case CountSelectedChoices(_)           => Valid
      case ChoicesCount(_)                   => Valid
      case TaskStatus(_)                     => Valid
      case LookupOps(_, _)                   => Valid
    }
  }

  def validateEmailParameter(formTemplate: FormTemplate): ValidationResult =
    formTemplate.emailParameters.fold[ValidationResult](Valid) { emailParams =>
      val ids = fieldIds(formTemplate.formKind.allSections)
      emailParams
        .collect {
          case EmailParameter(_, FormCtx(value)) if !ids.contains(value) => value
        } match {
        case Nil => Valid
        case invalidFields =>
          Invalid(
            s"The following email parameters are not fields in the form template's sections: ${invalidFields.mkString(", ")}"
          )
      }
    }

  def validateDates(formTemplate: FormTemplate): ValidationResult =
    getAllDates(formTemplate)
      .map {
        case ConcreteDate(Year.Exact(year), Month.Exact(month), Day.Exact(day)) =>
          validateYearMonthAndDay(year, month, day)
        case ConcreteDate(Year.Any, Month.Exact(month), Day.Exact(day)) =>
          val leapYear = 2020 //makes 29th of feb valid when we dont know the year
          validateYearMonthAndDay(leapYear, month, day)
        case _ => ""
      }
      .filterNot(_ === "") match {
      case Nil      => Valid
      case messages => Invalid(messages.mkString(". "))
    }

  private def validateYearMonthAndDay(year: Int, month: Int, day: Int): String =
    Try(LocalDate.of(year, month, day)) match {
      case Failure(message) => message.toString
      case Success(_)       => ""
    }

  private val isRevealingChoice: FormComponent => Boolean = fc =>
    fc.`type` match {
      case _: RevealingChoice => true
      case _                  => false
    }

  private val isGroup: FormComponent => Boolean = fc =>
    fc.`type` match {
      case group: Group => true
      case _            => false
    }

  def validateGroup(formTemplate: FormTemplate): ValidationResult =
    validateComponents("Group", formTemplate)(f => { case Group(fields, _, _, _, _) =>
      fields.forall(f)
    })

  def validateRevealingChoiceHint(formTemplate: FormTemplate): ValidationResult = {
    val formComponents: List[FormComponent] =
      SectionHelper.pages(formTemplate.formKind.allSections).flatMap(_.fields)

    val invalidResults: List[ValidationResult] = formComponents.map { c =>
      c.`type` match {
        case r: RevealingChoice
            if c.presentationHint.toList.flatten
              .contains(SeparateInSummary) && r.options.filter(_.revealingFields.size > 1).nonEmpty =>
          Invalid(s"separateInSummary can only be used where there is no more than one revealed fields for ${c.id}")
        case _ => Valid
      }
    }
    Monoid.combineAll(invalidResults)
  }

  def validateRevealingChoice(formTemplate: FormTemplate): ValidationResult =
    validateComponents("Revealing choice", formTemplate)(f => { case RevealingChoice(options, _) =>
      options.forall(_.revealingFields.forall(f))
    })

  private def validateComponents(str: String, formTemplate: FormTemplate)(
    pf: (FormComponent => Boolean) => PartialFunction[ComponentType, Boolean]
  ): ValidationResult = {

    val formComponents: List[FormComponent] =
      SectionHelper.pages(formTemplate.formKind.allSections).flatMap(_.fields)

    val rcElements: (FormComponent => Boolean) => Boolean = f =>
      formComponents.map(_.`type`).collect(pf(f)).forall(identity)

    val noRevealingChoice = rcElements(fc => !isRevealingChoice(fc))
    val noGroup = rcElements(fc => !isGroup(fc))

    (noRevealingChoice, noGroup) match {
      case (true, true) => Valid
      case (false, _)   => Invalid(str + " cannot contains revealing choice as its element")
      case (_, false)   => Invalid(str + " cannot contains group as its element")
    }
  }

  def validateEmailVerification(formTemplate: FormTemplate): ValidationResult = {

    val sections: List[Section] = formTemplate.formKind.allSections

    val indexLookup: Map[FormComponentId, Int] = indexedFieldIds(sections).toMap

    val allFormComponents: List[FormComponent] = indexedFields(sections).map { case (a, b) => a }

    val emailsWithCodes: List[(FormComponentId, FormComponentId)] =
      allFormComponents.collect { case IsEmailVerifiedBy(fcId, emailId) => (fcId, emailId) }

    val result = emailsWithCodes.map { case (emailField, codeField) =>
      val emailIdx = indexLookup.get(emailField) // This is guaranteed to always succeed
      val codeIdx = indexLookup.get(codeField)

      val res = for {
        e <- emailIdx
        c <- codeIdx
      } yield (e <= c).validationResult(
        s"id '$codeField' named in email verification is forward reference, which is not permitted"
      )

      res.getOrElse(Invalid(s"id '$codeField' named in email verification does not exist in the form"))
    }
    Monoid[ValidationResult].combineAll(result)
  }

  def validateAddToListAddAnotherQuestion(formTemplate: FormTemplate): ValidationResult = {

    def checkChoiceOptions(addToList: Section.AddToList): List[ValidationResult] = {
      implicit val l: LangADT = LangADT.En
      val invalid: Invalid = Invalid(
        s"AddToList '${addToList.title.defaultRawValue}' addAnotherQuestion must only contain choices of 'Yes' and 'No' in that order."
      )

      def isValid(t: Boolean): ValidationResult = if (t) Valid else invalid

      addToList.addAnotherQuestion match {
        case IsChoice(choice) =>
          choice.options.zipWithIndex.map {
            case (opt: OptionData.IndexBased, idx) if idx === 0 => isValid(opt.label.defaultRawValue === "Yes")
            case (opt: OptionData.IndexBased, idx) if idx === 1 => isValid(opt.label.defaultRawValue === "No")
            case _                                              => invalid
          }.toList
        case _ =>
          List(
            Invalid(s"AddToList '${addToList.title.defaultRawValue}' addAnotherQuestion must be a Choice component.")
          )
      }
    }

    val isATLChoiceOptionsValid: List[ValidationResult] =
      formTemplate.formKind.allSections.collect { case atl: Section.AddToList =>
        checkChoiceOptions(atl)
      }.flatten

    isATLChoiceOptionsValid.combineAll
  }

  def validateDraftRetrieval(formTemplate: FormTemplate): ValidationResult =
    formTemplate.draftRetrieval
      .flatMap { dr =>
        dr.mapping
          .get(AffinityGroup.Agent)
          .collect { case BySubmissionReference =>
            Invalid("The draft retrieve method 'submissionReference' is invalid for the user type 'agent'")
          }
          .orElse {
            dr.mapping.get(AffinityGroup.Individual).collect { case FormAccessCode(_) =>
              Invalid("The draft retrieve method 'accessCode' is invalid for the user type 'individual'")
            }
          }
      }
      .getOrElse(Valid)

  def validateIncludeIfForTaskStatus(formTemplate: FormTemplate, sections: List[Section]): ValidationResult = {
    val allTaskStatus = uk.gov.hmrc.gform.sharedmodel.form.TaskStatus.statuses.map(_.asString)
    def validateTaskStatus(booleanExpr: BooleanExpr, location: String): ValidationResult = {
      def reason(value: String) =
        s"'$value' is not valid for the 'taskStatus' expression in $location. It can be [${allTaskStatus.mkString(", ")}]."

      booleanExpr match {
        case Equals(TaskStatus(_), Constant(value)) if !allTaskStatus.contains(value) =>
          Invalid(reason(value))
        case Not(Equals(TaskStatus(_), Constant(value))) if !allTaskStatus.contains(value) =>
          Invalid(reason(value))
        case Or(left, right) =>
          Monoid[ValidationResult]
            .combineAll(List(validateTaskStatus(left, location), validateTaskStatus(right, location)))
        case And(left, right) =>
          Monoid[ValidationResult]
            .combineAll(List(validateTaskStatus(left, location), validateTaskStatus(right, location)))
        case _ => Valid
      }
    }

    Monoid[ValidationResult]
      .combineAll(
        SectionHelper
          .pages(sections)
          .zipWithIndex
          .flatMap { case (page: Page, idx) =>
            val formComponentConditions = page.allFormComponents.flatMap { fc =>
              fc.includeIf match {
                case Some(includeIf) => Some(includeIf.booleanExpr -> s"'${fc.id.value}' form component's includeIf")
                case _               => None
              }
            }

            val sectionConditions = page.includeIf.map(i => i.booleanExpr -> s"${idx + 1}. section's includeIf") ++
              page.removeItemIf.map(r => r.booleanExpr -> s"${idx + 1}. section's removeIf")

            val dataRetrieveConditions = page.dataRetrieve.toList.flatMap(
              _.toList.flatMap(
                _.`if`
                  .map(i => i.booleanExpr -> s"${idx + 1}. section's if")
              )
            )

            val taskConditions = formTemplate.formKind.fold(_ => Seq.empty[(BooleanExpr, String)])(tasklist =>
              tasklist.sections.toList.flatMap(_.tasks.toList.zipWithIndex.flatMap { case (task, id) =>
                task.startIf.map(i => i.booleanExpr -> s"${id + 1}. task's startIf") ++
                  task.notRequiredIf.map(i => i.booleanExpr -> s"${id + 1}. task's notRequiredIf") ++
                  task.includeIf.map(i => i.booleanExpr -> s"${id + 1}. task's includeIf")
              })
            )
            (formComponentConditions ++ sectionConditions ++ taskConditions ++ dataRetrieveConditions).map {
              case (booleanExpr, location) =>
                validateTaskStatus(booleanExpr, location)
            }
          }
      )
  }

  def validateAddToListRepeatConfig(formTemplate: FormTemplate, pages: List[Page]): ValidationResult = {

    def checkRepeatsUntil(addToList: Section.AddToList): List[ValidationResult] = {
      implicit val l: LangADT = LangADT.En
      val invalid: Invalid = Invalid(
        s"AddToList '${addToList.title.defaultRawValue}' repeatsUntil cannot be determined by a choice component with dynamic options from a data retrieve."
      )
      addToList.repeatsUntil.fold(List.empty[ValidationResult])(incIf => checkBooleanExpr(incIf.booleanExpr, invalid))
    }

    def checkRepeatsWhile(addToList: Section.AddToList): List[ValidationResult] = {
      implicit val l: LangADT = LangADT.En
      val invalid: Invalid = Invalid(
        s"AddToList '${addToList.title.defaultRawValue}' repeatsWhile cannot be determined by a choice component with dynamic options from a data retrieve."
      )
      addToList.repeatsWhile.fold(List.empty[ValidationResult])(incIf => checkBooleanExpr(incIf.booleanExpr, invalid))
    }

    def checkBooleanExpr(bExpr: BooleanExpr, invalid: Invalid): List[ValidationResult] =
      bExpr match {
        case Not(Equals(ChoicesSelected(FormComponentId(value)), ChoicesAvailable(FormComponentId(_), _))) =>
          if (areChoiceOptionsDataRetrievedBased(getChoiceById(value))) List(invalid) else List(Valid)
        case Equals(ChoicesSelected(FormComponentId(value)), ChoicesAvailable(FormComponentId(_), _)) =>
          if (areChoiceOptionsDataRetrievedBased(getChoiceById(value))) List(invalid) else List(Valid)
        case _ => List(Valid)
      }

    def getChoiceById(id: String): Option[Choice] =
      allFormComponents(pages)
        .map(fv => (fv.id, fv.`type`))
        .collectFirst {
          case (fId, choice: Choice) if fId.value === id =>
            choice
        }

    val isATLChoiceOptionsValid: List[ValidationResult] =
      formTemplate.formKind.allSections.collect { case atl: Section.AddToList =>
        checkRepeatsUntil(atl).appendedAll(checkRepeatsWhile(atl))
      }.flatten

    isATLChoiceOptionsValid.combineAll
  }

  def validateAddToListDefaultPage(formTemplate: FormTemplate): ValidationResult = {

    def checkComponentTypes(page: Page): List[ValidationResult] = {
      val reason =
        s"AddToList, '${page.title.defaultRawValue(LangADT.En)}', cannot contains fields in defaultPage other than Info type. " +
          s"All fields in defaultPage for AddToList must be Info or Table or MiniSummary type."

      page.fields map { fc => isViewOnlyComponent(fc, formTemplate, reason) }
    }

    val isNonInformationMessagePresent: List[ValidationResult] =
      formTemplate.formKind.allSections.flatMap {
        case s: Section.AddToList => s.defaultPage.toList.flatMap(checkComponentTypes)
        case _                    => Nil
      }

    isNonInformationMessagePresent.combineAll
  }

  def validateSummarySection(formTemplate: FormTemplate): ValidationResult = {

    def reasonHideAllRows(fc: FormComponent) =
      s"Field '${fc.id}' is not Info or Table or MiniSummary. All fields in 'summarySection' must be Info or Table or MiniSummary type."

    def reason(fc: FormComponent) =
      s"Field '${fc.id}' is not Info field. All fields in 'summarySection' must be Info (unless you specify 'hideDefaultRows': true)."

    def checkComponentType(summarySection: SummarySection) =
      if (summarySection.hideDefaultRows.getOrElse(false)) {
        summarySection.fields.fold(List.empty[ValidationResult])(fields =>
          fields.toList.map {
            case fc @ IsInformationMessage(i) if i.infoType == PdfLink => Invalid(pdfLinkNotAllowed(fc))
            case fc                                                    => isViewOnlyComponent(fc, formTemplate, reasonHideAllRows(fc))
          }
        )
      } else {
        summarySection.fields.fold(List.empty[ValidationResult])(fields =>
          fields.toList.map {
            case fc @ IsInformationMessage(i) if i.infoType == PdfLink => Invalid(pdfLinkNotAllowed(fc))
            case IsInformationMessage(_)                               => Valid
            case fc                                                    => Invalid(reason(fc))
          }
        )
      }

    def validatePdfLinkExpr(summarySection: SummarySection) = {
      val smartStrings = summarySection.fields.fold(List.empty[SmartString])(f =>
        f.collect { case IsInformationMessage(fc) =>
          fc.infoText
        }
      ) :+ summarySection.footer :+ summarySection.header

      smartStrings.flatMap(_.internals).map { s =>
        if (s.localised.m.values.exists(_.contains("/submissions/summary/pdf/")))
          Invalid(
            "`/submissions/summary/pdf/${form.id}` is deprecated. Please use `${link.printSummaryPdf}` instead"
          )
        else Valid
      }
    }

    formTemplate.formKind
      .fold[List[ValidationResult]] { _ =>
        val summarySection = formTemplate.summarySection
        checkComponentType(summarySection) ++ validatePdfLinkExpr(summarySection)
      } { taskList =>
        val summarySection = formTemplate.summarySection
        checkComponentType(summarySection) ++ validatePdfLinkExpr(summarySection) ++
          taskList.sections.toList.flatMap(
            _.tasks.toList
              .flatMap(_.summarySection.map(summarySection => checkComponentType(summarySection).combineAll))
          )
      }
      .combineAll
  }

  def validateAddToListCYAPage(formTemplate: FormTemplate): ValidationResult = {
    def checkComponentType(field: FormComponent): ValidationResult = {
      val reason =
        s"All fields in Check Your Answer page for AddToList must be Info or Table or MiniSummary type. Field Id, '${field.id}' is not an info field, table or mini summary."

      field match {
        case IsInformationMessage(InformationMessage(PdfLink, _, _)) => Invalid(pdfLinkNotAllowed(field))
        case _                                                       => isViewOnlyComponent(field, formTemplate, reason)
      }
    }

    formTemplate.formKind.allSections
      .collect { case s: Section.AddToList =>
        s.cyaPage.fold(List.empty[FormComponent])(_.fields.fold(List.empty[FormComponent])(_.toList))
      }
      .flatten
      .map(checkComponentType)
      .combineAll
  }

  def validateAddToListDeclarationSection(formTemplate: FormTemplate): ValidationResult = {
    def checkComponentType(field: FormComponent): ValidationResult = {
      val reason =
        s"All fields in Declaration section for AddToList must be Info or Table or MiniSummary type. Field Id, '${field.id}' is not an info field, table or mini summary."

      field match {
        case IsInformationMessage(InformationMessage(PdfLink, _, _)) => Invalid(pdfLinkNotAllowed(field))
        case _                                                       => isViewOnlyComponent(field, formTemplate, reason)
      }
    }

    def invalid(fcId: FormComponentId) = Invalid(
      s"AddToList declaration section for '${fcId.value}' can only be defined when a CYA page exists."
    )

    val declarationCheckDetails: List[(FormComponentId, Option[CheckYourAnswersPage], Option[DeclarationSection])] =
      formTemplate.formKind.allSections
        .collect { case s: Section.AddToList => (s.addAnotherQuestion.id, s.cyaPage, s.declarationSection) }

    val cyaResult = declarationCheckDetails.map {
      case (fcId, maybeCya, maybeDeclaration) if maybeCya.isEmpty && maybeDeclaration.isDefined => invalid(fcId)
      case _                                                                                    => Valid
    }

    val allowedComponentResult = declarationCheckDetails
      .flatMap(_._3.fold(List.empty[FormComponent])(_.fields))
      .map(checkComponentType)

    (cyaResult ++ allowedComponentResult).combineAll
  }

  def validateDataRetrieve(formTemplate: FormTemplate, pages: List[Page]): ValidationResult = {
    val pageDataRetrieves = pages.flatMap(_.dataRetrieves()).map(_.id)
    val ids = formTemplate.dataRetrieve.fold(pageDataRetrieves)(dr => pageDataRetrieves ++ dr.map(_.id).toList)
    val duplicates = ids.groupBy(identity).collect { case (fId, List(_, _, _*)) => fId }.toSet
    duplicates.isEmpty.validationResult(
      s"Some data retrieve ids are defined more than once: ${duplicates.toList.sortBy(_.value).map(_.value)}"
    )
  }

  def validatePagesToRevisit(sections: List[Section]): ValidationResult = {
    val allFields: List[(FormComponent, Int)] = indexedFields(sections)
    val pagesWithId: Map[PageId, Int] = SectionHelper
      .pages(sections)
      .zipWithIndex
      .collect {
        case (page: Page, idx: Int) if page.id.isDefined => page.id.get -> idx
      }
      .toMap

    allFields
      .flatMap { case (fc: FormComponent, idx: Int) =>
        fc.pageIdsToDisplayOnChange.map { pageList =>
          pageList.map { pageId =>
            (pagesWithId.contains(pageId), pagesWithId.get(pageId)) match {
              case (false, _)               => Invalid(s"Page with ID '${pageId.id}' not found in form template")
              case (_, Some(p)) if p < idx  => Invalid(s"Cannot revisit '${pageId.id}' as it's an earlier page in form")
              case (_, Some(p)) if p == idx => Invalid(s"Cannot revisit '${pageId.id}' as self")
              case (_, _)                   => Valid
            }
          }
        }
      }
      .flatten
      .combineAll
  }

  def validateDataRetrieveForwardReferences(sections: List[Section]): ValidationResult = {
    val dataRetrievesWithPageId: List[(DataRetrieve, Int)] = SectionHelper
      .pages(sections)
      .zipWithIndex
      .flatMap { case (page, idx) =>
        page.dataRetrieves().map(d => (d, idx))
      }

    val allReferencesWithPageId: Map[String, Int] = indexedFieldIds(sections).map { case (formComponentId, idx) =>
      formComponentId.value -> idx
    }.toMap ++ dataRetrievesWithPageId.map { case (dataRetrieve, idx) =>
      dataRetrieve.id.value -> idx
    }.toMap

    dataRetrievesWithPageId.map { case (dataRetrieve, pageIdx) =>
      val paramReferences: List[String] = dataRetrieve.params.collect {
        case DataRetrieve.ParamExpr(_, FormCtx(fcId))            => List(fcId.value)
        case DataRetrieve.ParamExpr(_, LookupColumn(fcId, _))    => List(fcId.value)
        case DataRetrieve.ParamExpr(_, DataRetrieveCtx(drId, _)) => List(drId.value)
        case DataRetrieve.ParamExpr(_, DataRetrieveCount(drId))  => List(drId.value)
        case DataRetrieve.ParamExpr(_, DateCtx(value)) =>
          value.leafExprs.collect {
            case FormCtx(fcId)                         => fcId.value
            case DateCtx(DataRetrieveDateCtx(drId, _)) => drId.value
          }
      }.flatten

      val forwardReferences: List[String] = paramReferences.filter { referenceId =>
        allReferencesWithPageId.get(referenceId).exists(_ > pageIdx)
      }

      forwardReferences.isEmpty.validationResult(
        s"Data retrieve with id '${dataRetrieve.id.value}' contains forward references to [${forwardReferences.mkString(",")}]"
      )
    }.combineAll
  }

  def validateDataRetrieveCtx(
    formTemplate: FormTemplate,
    pages: List[Page],
    allExpressions: List[ExprWithPath]
  ): ValidationResult = {

    val referenceInfos: List[DataRetrieveCtxExpr] =
      allExpressions.flatMap(_.referenceInfos).collect { case d: DataRetrieveCtxExpr =>
        d
      }

    val pageDataRetrieves: Map[DataRetrieveId, DataRetrieve] =
      pages.flatMap(p => p.dataRetrieves().map(d => (d.id, d))).toMap

    val dataRetrieves =
      formTemplate.dataRetrieve.fold(pageDataRetrieves)(d => pageDataRetrieves ++ d.toList.map(r => (r.id, r)).toMap)

    referenceInfos.map { r =>
      val maybeDataRetrieve = dataRetrieves.get(r.dataRetrieveCtx.id)
      maybeDataRetrieve.fold[ValidationResult](
        Invalid(s"Data retrieve expression at path ${r.path} refers to non-existent id ${r.dataRetrieveCtx.id.value}")
      ) { d =>
        val validAttributes =
          d.attributes.attributes :++ ((d.maxFailedAttempts, d.failureCountResetMinutes) match {
            case (Some(_), Some(_)) => DataRetrieve.reservedAttributes
            case _                  => List.empty[Attribute]
          })

        validAttributes
          .contains(r.dataRetrieveCtx.attribute)
          .validationResult {
            if (DataRetrieve.reservedAttributes.contains(r.dataRetrieveCtx.attribute))
              s"Data retrieve expression at path ${r.path}, with id '${r.dataRetrieveCtx.id.value}', refers to special attribute '${r.dataRetrieveCtx.attribute.name}', which may only be used if also specifying 'maxFailedAttempts' and 'failureCountResetMinutes' on the dataRetrieve."
            else {
              val validAttributesStr = validAttributes.map(_.name).mkString(", ")
              s"Data retrieve expression at path ${r.path}, with id '${r.dataRetrieveCtx.id.value}', refers to non-existent attribute '${r.dataRetrieveCtx.attribute.name}'. Valid attributes are: $validAttributesStr"
            }
          }
      }
    }.combineAll
  }

  def validateDataRetrieveCount(
    formTemplate: FormTemplate,
    pages: List[Page],
    allExpressions: List[ExprWithPath]
  ): ValidationResult = {

    val referenceInfos: List[DataRetrieveCountExpr] =
      allExpressions.flatMap(_.referenceInfos).collect { case d: DataRetrieveCountExpr =>
        d
      }

    val pageDataRetrieves: Set[DataRetrieveId] = pages.flatMap(p => p.dataRetrieves().map(_.id)).toSet

    val dataRetrieves: Set[DataRetrieveId] =
      formTemplate.dataRetrieve.fold(pageDataRetrieves)(d => pageDataRetrieves ++ d.toList.map(_.id))

    referenceInfos.map { r =>
      if (dataRetrieves(r.dataRetrieveCount.id))
        Valid
      else
        Invalid(s"Data retrieve expression at path ${r.path} refers to non-existent id ${r.dataRetrieveCount.id.value}")
    }.combineAll
  }

  def validateConfirmations(
    pages: List[Page]
  ): ValidationResult = {

    val cantConfirmFields: Set[FormComponentId] = pages
      .flatMap(_.allFormComponents)
      .collect {
        case fc @ IsInformationMessage(_) => fc.id
        case fc @ IsTable(_)              => fc.id
        case fc @ IsMiniSummaryList(_)    => fc.id
      }
      .toSet

    val confirmationRelatedData: List[(Option[PageId], Option[Confirmation])] = pages.map { page =>
      page.id -> page.confirmation
    }

    val confirmationValidation =
      confirmationRelatedData.foldLeft(ConfirmationPageValidation(Set.empty, cantConfirmFields, Nil)) {
        case (acc, (Some(pageId), None))    => acc.addPageId(pageId)
        case (acc, (_, Some(confirmation))) => acc.validateConfirmation(confirmation)
        case (acc, (None, None))            => acc
      }

    confirmationValidation.validationResult.combineAll

  }

  def validateSubmitSection(formTemplate: FormTemplate): ValidationResult = {
    val invalidReason = "Section 'submitSection' is required by the task list"
    formTemplate.formKind.fold { _ =>
      if (formTemplate.submitSection.isDefined) {
        Invalid(invalidReason)
      } else {
        Valid
      }
    } { _ =>
      if (formTemplate.submitSection.isEmpty) {
        Invalid(invalidReason)
      } else {
        Valid
      }
    }
  }

  private def isViewOnlyComponent(field: FormComponent, formTemplate: FormTemplate, reason: String): ValidationResult =
    field.`type` match {
      case _: InformationMessage => Valid
      case _: TableComp          => Valid
      case msl: MiniSummaryList  => validateMiniSummaryList(msl.rows, formTemplate)
      case _                     => Invalid(reason)
    }

  def validateAddToListInfoFields(formTemplate: FormTemplate): ValidationResult = {

    def reason(fc: FormComponent) =
      s"AddToList.fields must be Info or Table or MiniSummary types. Field '${fc.id.value}' is not an expected type."

    val isNonInformation: List[ValidationResult] =
      formTemplate.formKind.allSections.map {
        case s: Section.AddToList =>
          s.fields.fold[ValidationResult](Valid)(fields =>
            fields.toList.map(f => isViewOnlyComponent(f, formTemplate, reason(f))).combineAll
          )
        case _ => Valid
      }

    isNonInformation.combineAll
  }

  def validateTaskListDisplayWidth(formTemplate: FormTemplate): ValidationResult =
    formTemplate.formKind.fold(_ =>
      if (formTemplate.displayWidth.isDefined) Invalid("displayWidth property can only be used with task list")
      else Valid
    )(_ => Valid)

  def validateTaskListDeclarationSection(formTemplate: FormTemplate): ValidationResult = {

    def reason(fc: FormComponent) =
      s"""A declarationSection in a task list cannot contain enterable fields. Field '${fc.id.value}' is not Info or Mini Summary or Table field."""

    formTemplate.formKind.fold[ValidationResult](_ => Valid) { taskList =>
      taskList.sections.toList.flatMap { section =>
        section.tasks.toList.flatMap { task =>
          if (task.declarationSection.isDefined && task.summarySection.isEmpty)
            Seq(Invalid(s"""A destinationSection requires a summarySection in a task list."""))
          else
            task.declarationSection
              .map(ds =>
                ds.fields.map {
                  case fc @ IsInformationMessage(InformationMessage(PdfLink, _, _)) => Invalid(pdfLinkNotAllowed(fc))
                  case fc                                                           => isViewOnlyComponent(fc, formTemplate, reason(fc))
                }
              )
              .combineAll
        }
      }.combineAll
    }
  }

  def validateDataThreshold(sectionsList: List[Page]): ValidationResult = {
    val textAreaFieldIds = allFormComponents(sectionsList).collect {
      case fc @ IsTextArea(textArea) if textArea.dataThreshold.fold(false)(v => v > 100) => fc.id
    }

    textAreaFieldIds.isEmpty.validationResult(
      "'dataThreshold' is a percentage with a maximum of 100 for component Ids :" + textAreaFieldIds.mkString(",")
    )
  }

  def validateFileUpload(formTemplate: FormTemplate, appConfig: AppConfig): ValidationResult = {
    val formFileSizeLimit = formTemplate.fileSizeLimit.getOrElse(appConfig.formMaxAttachmentSizeMB)
    val formAllowedFileTypes = formTemplate.allowedFileTypes

    val fileUploads: List[(FormComponent, FileUpload)] = formTemplate.formComponents { case fc @ IsFileUpload(fu) =>
      fc -> fu
    }

    val fileSizeValidation: List[ValidationResult] = fileUploads.map { case (fc, fileUpload) =>
      fileUpload.fileSizeLimit
        .collect {
          case fileSizeLimit if fileSizeLimit > formFileSizeLimit =>
            Invalid(s"`${fc.id}` fileSizeLimit is larger than form fileSizeLimit of ${formFileSizeLimit}MB")
        }
        .getOrElse(Valid)
    }

    val allowedFileTypesValidation: List[ValidationResult] = fileUploads.map { case (fc, fileUpload) =>
      fileUpload.allowedFileTypes
        .collect {
          case allowedFileTypes
              if allowedFileTypes.fileExtensions.exists(!formAllowedFileTypes.fileExtensions.toList.contains(_)) =>
            Invalid(s"`${fc.id}` allowedFileTypes specify files not allowed at form level")
        }
        .getOrElse(Valid)
    }

    (fileSizeValidation ++ allowedFileTypesValidation).combineAll
  }

  def validateChoicesRevealedField(formTemplate: FormTemplate): ValidationResult =
    formTemplate.formKind.allSections.map {
      case a: Section.NonRepeatingPage =>
        LeafExpr(TemplatePath.root, a)
          .flatMap(_.referenceInfos)
          .map {
            case ReferenceInfo.ChoicesRevealedFieldExpr(TemplatePath(path), ChoicesRevealedField(fcId)) =>
              Invalid(
                s"$path: choicesRevealedField($fcId) is not invalid. Add to list choices cannot be replayed outside of Add to list"
              )
            case _ => Valid
          }
          .combineAll
      case _ => Valid
    }.combineAll

  def validateChoiceFormCtxOptionValues(sectionsList: List[Page], formTemplate: FormTemplate): ValidationResult = {
    val atlIds: List[FormComponentId] =
      SectionHelper.addToListFormComponents(formTemplate.formKind.allSections).map(_.id)

    def choiceFormCtxIds(choice: Choice): List[FormComponentId] = choice.options.collect {
      case OptionData
            .ValueBased(
              _,
              _,
              _,
              Some(Dynamic.ATLBased(fcId)),
              OptionDataValue.ExprBased(FormCtx(formComponentId)),
              _,
              _
            ) if fcId === formComponentId =>
        formComponentId
    }

    allFormComponents(sectionsList)
      .map(fv => (fv.id, fv.`type`))
      .map {
        case (fId, choice: Choice)
            if choiceFormCtxIds(choice).nonEmpty && !choiceFormCtxIds(choice).exists(atlIds.contains) =>
          Invalid(
            s"The value of '$fId' is not valid. The component expression value must be within the scope of an ATL."
          )
        case _ => Valid
      }
      .combineAll
  }

  private def areChoiceOptionsDataRetrievedBased(choice: Option[Choice]): Boolean =
    choice.fold(false)(c =>
      c.options.exists {
        case OptionData.IndexBased(_, _, _, d, _)       => d.fold(false)(d => isDataRetrieveBased(d))
        case OptionData.ValueBased(_, _, _, d, _, _, _) => d.fold(false)(d => isDataRetrieveBased(d))
      }
    )

  private def isDataRetrieveBased(dynamic: Dynamic): Boolean =
    dynamic match {
      case DataRetrieveBased(_) => true
      case _                    => false
    }
}

object IsEmailVerifiedBy {
  def unapply(formComponent: FormComponent): Option[(FormComponentId, FormComponentId)] = formComponent.`type` match {
    case Text(EmailVerifiedBy(fcId, _), _, _, _, _, _, _) => Some((formComponent.id, fcId))
    case _                                                => None
  }
}

final case class ConfirmationPageValidation(
  pageIds: Set[PageId],
  cantConfirmFields: Set[FormComponentId],
  validationResult: List[ValidationResult]
) {
  def addPageId(pageId: PageId) = this.copy(pageIds = pageIds + pageId)
  def validateConfirmation(confirmation: Confirmation) = {
    val confirmationId = confirmation.question.id
    val errorPrefix = s"Invalid confirmation: '${confirmationId.value}'."
    val mandatoryFields =
      if (!confirmation.fieldsConfirmed.isDefined && !confirmation.expressionsConfirmed.isDefined) {
        Invalid(
          s"$errorPrefix One of 'fieldsConfirmed' or 'expressionsConfirmed' fields must be present."
        )
      } else Valid
    val confirmationPageIdValidations: List[ValidationResult] = confirmation.redirects.toList
      .flatMap(_.toList)
      .map(_.pageId)
      .map(pageId =>
        if (pageIds.contains(pageId))
          Valid
        else
          Invalid(
            s"$errorPrefix No confirmation pageId: ${pageId.id} found."
          )
      )

    val invalidFieldBeingConfirmed = confirmation.fieldsConfirmed.fold[List[ValidationResult]](List(Valid)) {
      fieldsConfirmed =>
        fieldsConfirmed.toList.map { fcId =>
          if (cantConfirmFields(fcId)) {
            Invalid(
              s"$errorPrefix Cannot confirm field: '$fcId'. Components of types: info, miniSummaryList or table cannot be confirmed."
            )
          } else if (fcId === confirmationId) {
            Invalid(
              s"$errorPrefix Cannot confirm ourselves. Please remove '$confirmationId' from fieldsConfirmed"
            )
          } else {
            Valid
          }
        }
    }

    this.copy(
      validationResult =
        mandatoryFields :: confirmationPageIdValidations ++ validationResult ++ invalidFieldBeingConfirmed
    )
  }
}
