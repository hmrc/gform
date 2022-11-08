/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.data.NonEmptyList

import java.time.LocalDate
import cats.Monoid
import cats.implicits._
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.core.{ Invalid, Valid, ValidationResult }
import uk.gov.hmrc.gform.core.ValidationResult.{ BooleanToValidationResultSyntax, validationResultMonoid }
import uk.gov.hmrc.gform.models.constraints.{ AddressLensChecker, FunctionsChecker, MutualReferenceChecker, ReferenceInfo }
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, DataRetrieve, DataRetrieveId, LangADT, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.DependencyGraph._
import uk.gov.hmrc.gform.formtemplate.FormTemplateValidatorHelper._
import uk.gov.hmrc.gform.models.constraints.ReferenceInfo.{ DataRetrieveCtxExpr, DateFunctionExpr, FormCtxExpr, PeriodExpr, PeriodExtExpr, SizeExpr }
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink

import scala.Function.const
import scala.util.{ Failure, Success, Try }

object FormTemplateValidator {

  def validateLowercaseIds(
    formTemplate: FormTemplate
  ): ValidationResult = {
    val _id = formTemplate._id.value
    val legacyFormIds = formTemplate.legacyFormIds.fold(List.empty[String])(_.map(_.value).toList)
    val version = formTemplate.version.fold("")(_.version)
    if (_id =!= _id.toLowerCase) {
      Invalid(
        "_id must be lowercase. Found: " + _id
      ) // TODO remove lowercasing logic from FormTemplateRaw for this to have an effect
    } else if (legacyFormIds =!= legacyFormIds.map(_.toLowerCase)) {
      Invalid("legacyFormIds must be lowercase. Found: " + legacyFormIds.mkString(", "))
    } else if (version =!= version.toLowerCase) {
      Invalid("version cannot contains uppercase letters. Found: " + version)
    } else {
      Valid
    }
  }

  def validateLanguages(languageList: AvailableLanguages): ValidationResult =
    if (languageList.languages.contains(LangADT.En)) Valid
    else Invalid("languages must contain en")

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
    val ids: List[PageId] =
      sectionsList.flatMap(
        _.fold(_.page.id.toList)(_.page.id.toList)(p =>
          p.defaultPage.flatMap(_.id).toList ++ p.pages.toList.flatMap(_.id)
        )
      )
    val duplicateIds = ids.groupBy(identity).collect {
      case (_, ids) if ids.size > 1 => ids.head
    }
    duplicateIds.isEmpty.validationResult(
      s"Some page ids are defined more than once: ${duplicateIds.toList.sortBy(_.id).map(_.id).mkString(",")}"
    )
  }

  def validateInstructions(pages: List[Page]): ValidationResult =
    if (!pages.flatMap(_.instruction).flatMap(_.name).forall(_.nonEmpty)) {
      Invalid("One or more sections have instruction attribute with empty names")
    } else if (!pages.flatMap(_.instruction.flatMap(_.order)).forall(_ >= 0)) {
      Invalid("One or more sections have instruction attribute with negative order")
    } else if (
      pages.flatMap(_.instruction.flatMap(_.order)).distinct.size != pages
        .flatMap(_.instruction.flatMap(_.order))
        .size
    ) {
      Invalid("One or more sections have instruction attribute with duplicate order value")
    } else if (!pages.flatMap(_.fields).flatMap(_.instruction).flatMap(_.name).forall(_.nonEmpty)) {
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
      val fc = SectionHelper.addToListFormComponents(formTemplate.formKind.allSections).filter(_.id == fcId).headOption
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

  def validateCsvOverseasCountryCheck(formTemplate: FormTemplate, pages: List[Page]): ValidationResult = {
    val allOverseasAddresses: List[FormComponentId] = allFormComponents(pages).collect {
      case fc @ IsOverseasAddress(_) => fc.id
    }
    val allExprs: List[ExprWithPath] = FormTemplate.leafExprs.exprs(TemplatePath.root, formTemplate)
    val results: List[ValidationResult] = allExprs.collect {
      case ExprWithPath(path, CsvOverseasCountryCheck(fcId, _)) =>
        if (allOverseasAddresses.contains(fcId))
          Valid
        else
          Invalid(s"$path: $fcId is not a overseas address component")
    }
    Monoid.combineAll(results)
  }

  def validateInvalidReferences(formTemplate: FormTemplate): ValidationResult = {

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

    val allExprs: List[ExprWithPath] = FormTemplate.leafExprs.exprs(TemplatePath.root, formTemplate)

    def dateExprInvalidRefs(dateExpr: DateExpr*): Seq[FormComponentId] =
      dateExpr.flatMap(
        _.maybeFormCtx.flatMap(fCtx => if (!allFcIds(fCtx.formComponentId)) List(fCtx.formComponentId) else List())
      )

    def invalid(path: TemplatePath, formComponentIds: FormComponentId*): Invalid =
      Invalid(s"${path.path}: ${formComponentIds.mkString(",")} doesn't exist in the form")

    allExprs.flatMap(_.referenceInfos).foldMap {
      case ReferenceInfo.FormCtxExpr(path, FormCtx(formComponentId)) if !allFcIds(formComponentId) =>
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
      case ReferenceInfo.SizeExpr(path, Size(formComponentId, _)) if !allFcIds(formComponentId) =>
        invalid(path, formComponentId)
      case ReferenceInfo.LinkCtxExpr(path, LinkCtx(PageLink(pageId))) if !allPageIds.contains(pageId) =>
        Invalid(s"${path.path}: Page id '${pageId.id}' doesn't exist in the form")
      case ReferenceInfo.CsvCountryCheckExpr(path, CsvCountryCheck(formComponentId, _)) if !allFcIds(formComponentId) =>
        invalid(path, formComponentId)
      case _ => Valid
    }
  }

  def validateSectionShortNames(formTemplate: FormTemplate): ValidationResult = {

    def checkEmpty(maybeSmartString: Option[SmartString]): Boolean =
      maybeSmartString.fold(false) { smartString =>
        smartString.localised.m.values.exists(_.trim.isEmpty)
      }

    def checkPage(page: Page): ValidationResult =
      if (checkEmpty(page.shortName)) {
        val title = page.title.rawValue(LangADT.En)
        Invalid(
          s"shortName is empty for title: '$title'. If you want to hide page title on summary page, use 'presentationHint': 'invisiblePageTitle' instead."
        )
      } else Valid

    val isEmptyShortNamePresent: List[ValidationResult] =
      formTemplate.formKind.allSections.flatMap {
        case s: Section.NonRepeatingPage => checkPage(s.page) :: Nil
        case s: Section.RepeatingPage    => checkPage(s.page) :: Nil
        case s: Section.AddToList        => s.pages.toList.map(page => checkPage(page))
      }

    isEmptyShortNamePresent.find(!_.isValid).getOrElse(Valid)

  }

  def validateReferencesConstraints(
    formTemplate: FormTemplate,
    allExpressions: List[ExprWithPath]
  ): ValidationResult = {
    val mrc = new MutualReferenceChecker(formTemplate)
    val functionsChecker = new FunctionsChecker(formTemplate, allExpressions)

    Monoid.combineAll(List(mrc.result, functionsChecker.result))
  }

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
      .fold[ValidationResult](Invalid(s"${path.path}: Form component $formComponentId is invalid"))(
        _.cast[Date]
          .fold[ValidationResult](
            Invalid(
              s"${path.path}: Form component '$formComponentId' used in $functionName function should be date type"
            )
          )(_ => Valid)
      )

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
    }
    Monoid.combineAll(validations.flatten)
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
    errorMessage: String
  ): ValidationResult = {
    val choiceFieldIds = allFormComponents(sectionsList)
      .map(fv => (fv.id, fv.`type`))
      .collect {
        case (fId, choice: Choice) if choiceChecker(choice) =>
          fId
      }

    choiceFieldIds.isEmpty.validationResult(errorMessage + ": " + choiceFieldIds.mkString(","))
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

  def validateChoiceDividerPositionLowerBound(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean = choice.dividerPosition.fold(false)(_ <= 0)

    validateChoice(sectionsList, check, "dividerPosition should be greater than 0")
  }

  def validateChoiceDividerPositionUpperBound(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean = choice.dividerPosition.fold(false)(_ >= choice.options.size)

    validateChoice(sectionsList, check, "dividerPosition should be less than the number of choices")
  }

  def validateChoiceNoneChoiceValue(sectionsList: List[Page]): ValidationResult = {
    def check(choice: Choice): Boolean = {
      val values = choice.options.collect { case OptionData.ValueBased(_, v, _) =>
        v
      }
      choice.noneChoice.fold(false) {
        case NoneChoice.IndexBased(index) => !values.isEmpty
        case NoneChoice.ValueBased(value) => !values.contains(value)
      }
    }

    validateChoice(sectionsList, check, "'noneChoice' should match one of choices 'value'")
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
    def check(choice: Choice): Boolean = choice.noneChoice.isDefined != choice.noneChoiceError.isDefined

    validateChoice(sectionsList, check, "noneChoice and noneChoiceError should be defined")
  }

  def validateChoiceOptions(sectionsList: List[Page]): ValidationResult = {
    def checkUniformness(choice: Choice): Boolean = {
      val indexed = choice.options.collect { case o: OptionData.IndexBased =>
        o
      }
      indexed.size =!= 0 && indexed.size =!= choice.options.size
    }

    def checkUnique(choice: Choice): Boolean = {
      val values = choice.options.collect { case OptionData.ValueBased(_, value, _) =>
        value
      }
      values.size =!= values.distinct.size
    }

    def noComma(choice: Choice): Boolean = {
      val values = choice.options.collect { case OptionData.ValueBased(_, value, _) =>
        value
      }
      values.size =!= 0 && values.exists(_.contains(","))
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
        noComma,
        "Choice component options 'value' cannot contain ',' (comma)"
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
      val values = choices.collect { case OptionData.ValueBased(_, value, _) =>
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

  def validateLabel(sectionsList: List[Page]): ValidationResult = {
    def validateChoiceSingleChoice(sectionsList: List[Page]): ValidationResult = {
      def check(choice: Choice): Boolean = choice.options.length <= 1

      validateChoice(sectionsList, check, "more than one options in the choice component")
    }
    def isEmptyLabel(label: SmartString): Boolean =
      label.rawValue(LangADT.En).trim == "" || label.rawValue(LangADT.Cy).trim == ""

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
          Invalid(s"choice address component ${fc.id} should have a non-blank label")
        case fc @ IsText(_) if isEmptyLabel(fc.label) && !fc.onlyShowOnSummary =>
          Invalid(s"text component ${fc.id} should have a non-blank label, unless submitMode is summaryinfoonly")
        case fc @ IsTextArea(_) if isEmptyLabel(fc.label) && !fc.onlyShowOnSummary =>
          Invalid(s"text area component ${fc.id} should have a non-blank label, unless submitMode is summaryinfoonly")
        case fc @ IsFileUpload(_) if isEmptyLabel(fc.label) =>
          Invalid(s"fileUpload component ${fc.id} should have a non-blank label")
      }
    }

    Monoid[ValidationResult].combineAll(validationResults)
  }

  def validatePostcodeLookup(sectionsList: List[Page]): ValidationResult = {
    val pagesWithPostcodeLookups: List[(FormComponentId, Page)] = sectionsList.flatMap { page =>
      page.fields.collect { case fc @ IsPostcodeLookup() =>
        fc.id -> page
      }
    }
    val noOtherComponentsAllowed: ValidationResult = pagesWithPostcodeLookups.foldMap { case (postcodeId, page) =>
      val filteredFields = page.fields.filter {
        case IsPostcodeLookup() | IsInformationMessage(_) => true
        case _                                            => false
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
      .collectFirst { case fc @ IsPostcodeLookup() =>
        fc
      }
      .fold[ValidationResult](Valid)(fc =>
        Invalid(s"Postcode lookup '${fc.id.value}' cannot be inside Revealing choice or Group")
      )

    val postcodeLookups: List[List[FormComponentId]] = sectionsList.map { page =>
      page.allFormComponents.collect { case fc @ IsPostcodeLookup() =>
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

    Monoid.combineAll(
      List(
        singlePostcodeLookupOnly,
        noPostcodeLookupInReveliangChoiceOrGroup,
        noOtherComponentsAllowed
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
      options.collect { case OptionData.ValueBased(_, index, _) =>
        index match {
          case SizeRefType.regex(_*) => Valid
          case _ =>
            Invalid(
              s"Wrong value '$index'. Only ${SizeRefType.regex.regex} characters are allowed"
            )
        }
      }

    def validateSize(
      formComponentId: FormComponentId,
      options: NonEmptyList[OptionData],
      index: SizeRefType
    ): ValidationResult = {
      val availableOptions = options.toList.collect { case OptionData.ValueBased(_, value, _) =>
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
      case HmrcEnrolmentModule(EnrolmentAuth(_, DoCheck(_, _, RegimeIdCheck(regimeId)))) => regimeIdCheck(regimeId)
      case HmrcAgentWithEnrolmentModule(_, EnrolmentAuth(_, DoCheck(_, _, RegimeIdCheck(regimeId)))) =>
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

  def validate(componentTypes: List[ComponentType], formTemplate: FormTemplate): ValidationResult = {
    val results = componentTypes.map(validate(_, formTemplate))
    Monoid[ValidationResult].combineAll(results)
  }

  def validate(componentType: ComponentType, formTemplate: FormTemplate): ValidationResult = componentType match {
    case HasExpr(SingleExpr(expr))            => validate(expr, formTemplate.formKind.allSections)
    case HasExpr(MultipleExpr(fields))        => Valid
    case Date(_, _, _)                        => Valid
    case CalendarDate                         => Valid
    case TaxPeriodDate                        => Valid
    case Address(_)                           => Valid
    case Choice(_, _, _, _, _, _, _, _, _, _) => Valid
    case RevealingChoice(revealingChoiceElements, _) =>
      validate(revealingChoiceElements.toList.flatMap(_.revealingFields.map(_.`type`)), formTemplate)
    case HmrcTaxPeriod(_, _, _)      => Valid
    case Group(fvs, _, _, _, _)      => validate(fvs.map(_.`type`), formTemplate)
    case FileUpload(_)               => Valid
    case InformationMessage(_, _)    => Valid
    case Time(_, _)                  => Valid
    case OverseasAddress(_, _, _, _) => Valid
    case PostcodeLookup              => Valid
    case MiniSummaryList(ls)         => validateMiniSummaryList(ls, formTemplate)
    case t: TableComp                => TableCompValidator.validateTableComp(t)
  }

  def validateMiniSummaryList(rows: List[MiniSummaryRow], formTemplate: FormTemplate): ValidationResult = {
    val atlIds = SectionHelper.addToListIds(formTemplate.formKind.allSections).map(_.id)
    Monoid.combineAll(rows.map {
      case MiniSummaryRow.ATLRow(atlId, _, _) =>
        if (atlIds.contains(atlId)) Valid else Invalid(s"$atlId is not AddToList Id")
      case _ => Valid
    })
  }

  def validateForwardReference(sections: List[Section]): ValidationResult = {
    val indexLookup: Map[FormComponentId, Int] = indexedFieldIds(sections).toMap
    val addToListIds: List[FormComponentId] = SectionHelper.addToListIds(sections).map(_.id)
    val verifyValidIf = new BooleanExprValidator(indexLookup, BooleanExprWrapperType.ValidIf, addToListIds)(_)
    val verifyIncludeIf = new BooleanExprValidator(indexLookup, BooleanExprWrapperType.IncludeIf, addToListIds)(_)
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

            verifiedValidIf ++ verifiedIncludeIf ++ verifiedComponentIncludeIfs
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
      case IfElse(cond, field1, field2) => checkFields(field1, field2) // cond is covered by LeafExpr
      case Else(field1, field2)         => checkFields(field1, field2)
      case Sum(value)                   => validate(value, sections)
      case Count(value)                 => validate(FormCtx(value), sections)
      case HmrcRosmRegistrationCheck(_) => Valid
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
      case PeriodExt(periodFun, _)           => validate(periodFun, sections)
      case DataRetrieveCtx(_, _)             => Valid
      case CsvCountryCheck(value, _)         => validate(FormCtx(value), sections)
      case CsvOverseasCountryCheck(value, _) => validate(FormCtx(value), sections)
      case CsvCountryCountCheck(value, _, _) =>
        SectionHelper
          .addToListIds(sections)
          .map(_.id)
          .contains(value)
          .validationResult(s"$value is not AddToList Id")
      case Size(value, _)   => validate(FormCtx(value), sections)
      case Typed(expr, tpe) => validate(expr, sections)
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
      .filterNot(_ == "") match {
      case messages if messages.isEmpty  => Valid
      case messages if messages.nonEmpty => Invalid(messages.mkString(". "))
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

  def validateAddToListDefaultPage(formTemplate: FormTemplate): ValidationResult = {

    def checkComponentTypes(page: Page): List[ValidationResult] = {
      val title = page.title.rawValue(LangADT.En)

      page.fields.map(_.`type`) map {
        case InformationMessage(_, _) => Valid
        case _ =>
          Invalid(
            s"AddToList, '$title', cannot contains fields in defaultPage other than Info type. " +
              s"All fields in defaultPage for AddToList must be Info type."
          )
      }
    }

    val isNonInformationMessagePresent: List[ValidationResult] =
      formTemplate.formKind.allSections.flatMap {
        case s: Section.AddToList => s.defaultPage.toList.flatMap(checkComponentTypes _)
        case _                    => Nil
      }

    isNonInformationMessagePresent.combineAll
  }

  def validateSummarySection(formTemplate: FormTemplate): ValidationResult = {

    def checkComponentTypes(fields: List[FormComponent]): List[ValidationResult] =
      fields.map {
        case IsInformationMessage(_) => Valid
        case IsMiniSummaryList(_)    => Valid
        case fc                      => Invalid(s"""Field '${fc.id}' is not Info field. All fields in 'summarySection' must be Info type.""")
      }

    val isNonInformationMessagePresent: List[ValidationResult] = checkComponentTypes(
      formTemplate.summarySection.fields.fold(List.empty[FormComponent])(_.toList)
    )

    isNonInformationMessagePresent.combineAll
  }

  def validateAddToListLimit(formTemplate: FormTemplate): ValidationResult = {

    def isInfo(field: FormComponent): ValidationResult =
      field.`type` match {
        case InformationMessage(_, _) => Valid
        case _ =>
          Invalid(
            s"AddToList.limit.field must be an Info type. Field '${field.id.value}' is not an Info type."
          )
      }

    val isNonInformationMessagePresent: List[ValidationResult] =
      formTemplate.formKind.allSections.map {
        case s: Section.AddToList => s.limit.fold[ValidationResult](Valid)(limit => isInfo(limit.field))
        case _                    => Valid
      }

    isNonInformationMessagePresent.combineAll
  }

  def validateDataRetrieve(pages: List[Page]): ValidationResult = {
    val ids = pages.flatMap(_.dataRetrieve).map(_.id)
    val duplicates = ids.groupBy(identity).collect { case (fId, List(_, _, _*)) => fId }.toSet
    duplicates.isEmpty.validationResult(
      s"Some data retrieve ids are defined more than once: ${duplicates.toList.sortBy(_.value).map(_.value)}"
    )
  }

  def validateDataRetrieveFormCtxReferences(pages: List[Page]): ValidationResult = {
    val dataRetrieves: List[(Page, DataRetrieve)] = pages.flatMap(p => p.dataRetrieve.map(d => (p, d)))
    dataRetrieves.map { case (page, dataRetrieve) =>
      val pageFormComponentsIds = page.allFormComponents.map(_.id)
      val formCtxExprs = dataRetrieve.formCtxExprs.collect { case ctx: FormCtx =>
        ctx
      }
      val nonExistentFCRefs = formCtxExprs.map(_.formComponentId).filterNot(pageFormComponentsIds.contains)
      nonExistentFCRefs.isEmpty.validationResult(
        s"Data retrieve with id '${dataRetrieve.id.value}' refers to form components that does not exist in the page [${nonExistentFCRefs.map(_.value).mkString(",")}]"
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

    val dataRetrieves: Map[DataRetrieveId, DataRetrieve] = pages.flatMap(p => p.dataRetrieve.map(d => (d.id, d))).toMap

    referenceInfos.map { r =>
      val maybeDataRetrieve = dataRetrieves.get(r.dataRetrieveCtx.id)
      maybeDataRetrieve.fold[ValidationResult](
        Invalid(s"Data retrieve expression at path ${r.path} refers to non-existent id ${r.dataRetrieveCtx.id.value}")
      ) { d =>
        d.attributes
          .contains(r.dataRetrieveCtx.attribute)
          .validationResult(
            s"Data retrieve expression at path ${r.path}, with id ${r.dataRetrieveCtx.id.value}, refers to non-existent attribute ${r.dataRetrieveCtx.attribute.name}"
          )
      }
    }.combineAll
  }

  def validateConfirmations(
    formTemplate: FormTemplate,
    pages: List[Page]
  ): ValidationResult = {
    val confirmationRelatedData: List[(Option[PageId], Option[Confirmation])] = pages.map { page =>
      page.id -> page.confirmation
    }

    val confirmationValidation = confirmationRelatedData.foldLeft(ConfirmationPageValidation(Set.empty, Nil)) {
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

  def validateAddToListInfoFields(formTemplate: FormTemplate): ValidationResult = {

    def isInfo(field: FormComponent): ValidationResult =
      field.`type` match {
        case InformationMessage(_, _) => Valid
        case _ =>
          Invalid(
            s"AddToList.fields must be Info types. Field '${field.id.value}' is not an Info type."
          )
      }

    val isNonInformation: List[ValidationResult] =
      formTemplate.formKind.allSections.map {
        case s: Section.AddToList =>
          s.fields.fold[ValidationResult](Valid)(fields => fields.toList.map(f => isInfo(f)).combineAll)
        case _ => Valid
      }

    isNonInformation.combineAll
  }

  def validateUpscanForObjectStore(formTemplate: FormTemplate, pages: List[Page]): ValidationResult = {

    def checkComponentTypes(fields: List[FormComponent]): List[ValidationResult] = {
      val fileUploads: List[(FileUpload, FormComponent)] = fields.collect { case f @ IsFileUpload(fileUpload) =>
        fileUpload -> f
      }
      fileUploads.map {
        case (FileUpload(FileUploadProvider.Upscan(_)), _) => Valid
        case (_, fc)                                       => Invalid(s"The service of file-upload component ${fc.id} must be Upscan for object-store")
      }
    }

    if (formTemplate.isObjectStore) {
      checkComponentTypes(pages.flatMap(_.allFormComponents)).combineAll
    } else Valid
  }
}

object IsEmailVerifiedBy {
  def unapply(formComponent: FormComponent): Option[(FormComponentId, FormComponentId)] = formComponent.`type` match {
    case Text(EmailVerifiedBy(fcId, _), _, _, _, _, _) => Some((formComponent.id, fcId))
    case _                                             => None
  }
}

final case class ConfirmationPageValidation(
  pageIds: Set[PageId],
  validationResult: List[ValidationResult]
) {
  def addPageId(pageId: PageId) = this.copy(pageIds = pageIds + pageId)
  def validateConfirmation(confirmation: Confirmation) = {
    val pageId = confirmation.pageId
    this.copy(
      validationResult =
        if (pageIds.contains(pageId))
          validationResult
        else
          Invalid(
            s"No pageId '${pageId.id}' found. Confirmation question '${confirmation.question.id.value}' should confirm pageId: '${pageId.id}'"
          ) :: validationResult
    )
  }
}
