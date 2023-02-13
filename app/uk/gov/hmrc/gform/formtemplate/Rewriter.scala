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

import cats.data.NonEmptyList
import cats.implicits._

import scala.util.{ Failure, Success, Try }
import uk.gov.hmrc.gform.core.{ FOpt, fromOptA }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationIncludeIf, Destinations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.IncludeIfValue
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.{ Email, HandlebarsHttpApi, HmrcDms, Log, StateTransition, SubmissionConsolidator }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList

trait Rewriter {
  def rewrite(formTemplate: FormTemplate): FOpt[FormTemplate] = fromOptA(validateAndRewriteBooleanExprs(formTemplate))

  private def mkComponentTypeLookup(formComponent: FormComponent): Map[FormComponentId, ComponentType] = {
    val mainComponent: Map[FormComponentId, ComponentType] = Map(formComponent.id -> formComponent.`type`)
    val subComponent: Map[FormComponentId, ComponentType] = formComponent match {
      case IsGroup(group) => group.fields.map(fc => fc.id -> fc.`type`).toMap
      case IsRevealingChoice(revealingChoice) =>
        revealingChoice.options.toList.flatMap(_.revealingFields).map(fc => fc.id -> fc.`type`).toMap
      case _ => Map.empty[FormComponentId, ComponentType]
    }
    mainComponent ++ subComponent
  }

  private def lookupFromPage(fields: List[FormComponent]): Map[FormComponentId, ComponentType] =
    fields.foldLeft(Map.empty[FormComponentId, ComponentType])((acc, fc) => mkComponentTypeLookup(fc) ++ acc)

  private def missingFormComponentId[A](formComponentId: FormComponentId): Either[UnexpectedState, A] =
    Left(UnexpectedState(s"Missing component with id $formComponentId"))

  private def nonNestedFormComponentValidIf(formComponent: FormComponent): List[ValidIf] =
    formComponent.validIf.toList ++ formComponent.validators.map(_.validIf)

  private def formComponentValidIf(formComponent: FormComponent): List[ValidIf] =
    fetchFromFormComponent(nonNestedFormComponentValidIf)(formComponent)

  private def formComponentIncludeIf(formComponent: FormComponent): List[IncludeIf] =
    fetchFromFormComponent(_.includeIf.toList)(formComponent)

  private def fetchFromFormComponent[A](f: FormComponent => List[A])(formComponent: FormComponent): List[A] =
    formComponent match {
      case fc @ IsGroup(group) => f(fc) ++ group.fields.flatMap(fetchFromFormComponent(f))
      case fc @ IsRevealingChoice(revealingChoice) =>
        f(fc) ++ revealingChoice.options.toList
          .flatMap(_.revealingFields)
          .flatMap(fetchFromFormComponent(f))
      case fc => f(fc)
    }

  private def validateAndRewriteBooleanExprs(formTemplate: FormTemplate): Either[UnexpectedState, FormTemplate] = {

    val fcLookupDeclaration: Map[FormComponentId, ComponentType] = formTemplate.destinations match {
      case dl: Destinations.DestinationList =>
        dl.declarationSection.fold(Map.empty[FormComponentId, ComponentType])(d => lookupFromPage(d.fields))
      case dp: Destinations.DestinationPrint => Map.empty[FormComponentId, ComponentType]
    }

    def questionFcid(page: Page) = lookupFromPage(page.confirmation.toList.map(_.question))

    val fcLookup: Map[FormComponentId, ComponentType] =
      formTemplate.formKind.allSections.foldLeft(fcLookupDeclaration) {
        case (acc, Section.NonRepeatingPage(page)) => acc ++ lookupFromPage(page.fields) ++ questionFcid(page)
        case (acc, Section.RepeatingPage(page, _)) => acc ++ lookupFromPage(page.fields) ++ questionFcid(page)
        case (acc, Section.AddToList(_, _, _, _, _, _, _, _, pages, _, _, _, _, _, _, _, _, _, _)) =>
          acc ++ pages.toList.flatMap(page => lookupFromPage(page.fields) ++ questionFcid(page))
      }

    val validIfsDeclaration = formTemplate.destinations match {
      case dl: Destinations.DestinationList =>
        dl.declarationSection.toList.flatMap(_.fields.flatMap(formComponentValidIf))
      case dp: Destinations.DestinationPrint => Nil
    }

    def traverseFormComponents[A](f: FormComponent => List[A]): List[A] =
      formTemplate.formKind.allSections.flatMap {
        case Section.NonRepeatingPage(page) => page.fields.flatMap(f)
        case Section.RepeatingPage(page, _) => page.fields.flatMap(f)
        case Section.AddToList(_, _, _, _, _, _, _, _, pages, _, _, _, _, _, _, _, _, _, _) =>
          pages.toList.flatMap(page => page.fields.flatMap(f))
      }

    val validIfs: List[ValidIf] = traverseFormComponents(formComponentValidIf) ++ validIfsDeclaration

    val fieldsIncludeIfs: List[IncludeIf] = traverseFormComponents(formComponentIncludeIf)

    val destinationsIncludeIfs: List[IncludeIf] = formTemplate.destinations match {
      case dl: DestinationList =>
        dl.acknowledgementSection.fields.flatMap(_.includeIf) ++ dl.declarationSection.fold(List.empty[IncludeIf])(
          _.fields.flatMap(_.includeIf)
        ) ++ dl.destinations.toList.collect {
          case HmrcDms(_, _, _, _, _, IncludeIfValue(includeIf), _, _, _, _, _)  => includeIf
          case HandlebarsHttpApi(_, _, _, _, _, _, IncludeIfValue(includeIf), _) => includeIf
          case Destination.Composite(_, IncludeIfValue(includeIf), _)            => includeIf
          case StateTransition(_, _, IncludeIfValue(includeIf), _)               => includeIf
          case SubmissionConsolidator(_, _, _, _, IncludeIfValue(includeIf), _)  => includeIf
          case Email(_, _, IncludeIfValue(includeIf), _, _, _)                   => includeIf
        }
      case _ => Nil
    }

    val summarySectionIncludeIfs: List[IncludeIf] =
      formTemplate.summarySection.fields.fold(List.empty[IncludeIf])(_.toList.flatMap(_.includeIf)) ++
        formTemplate.summarySection.includeIf ++
        formTemplate.formKind.fold(_ => List.empty[IncludeIf])(taskList =>
          taskList.sections.toList.flatMap(_.tasks.toList.flatMap(_.summarySection.flatMap(_.includeIf))) ++
            taskList.sections.toList.flatMap(
              _.tasks.toList.flatMap(
                _.summarySection.toList.flatMap(_.fields.toList.flatMap(_.toList.flatMap(_.includeIf)))
              )
            )
        )

    val choiceIncludeIfs: List[IncludeIf] = formTemplate.formKind.allSections.flatMap { section =>
      section
        .formComponents {
          case IsChoice(choice)                   => choice.options.toList
          case IsRevealingChoice(revealingChoice) => revealingChoice.options.toList.map(_.choice)
        }
        .flatten
        .collect {
          case OptionData.ValueBased(_, _, Some(includeIf)) => includeIf
          case OptionData.IndexBased(_, Some(includeIf))    => includeIf
        }
    }

    val miniSummaryListIncludeIfs: List[IncludeIf] = formTemplate.formKind.allSections.flatMap { section =>
      section
        .formComponents { case IsMiniSummaryList(summaryList) => summaryList.rows }
        .flatten
        .collect {
          case MiniSummaryRow.ValueRow(_, _, Some(includeIf))       => includeIf
          case MiniSummaryRow.SmartStringRow(_, _, Some(includeIf)) => includeIf
        }
    }

    val pages: List[Page] = formTemplate.formKind.allSections.flatMap {
      case Section.NonRepeatingPage(page)                                                 => List(page)
      case Section.RepeatingPage(page, _)                                                 => List(page)
      case Section.AddToList(_, _, _, _, _, _, _, _, pages, _, _, _, _, _, _, _, _, _, _) => pages.toList
    }

    val redirectsIncludeIfs: List[IncludeIf] = pages.flatMap(_.redirects).flatMap(_.toList.map(_.`if`))

    val confirmationRedirectsIncludeIfs: List[IncludeIf] =
      for {
        page         <- pages
        confirmation <- page.confirmation.toList
        redirect     <- confirmation.redirects.toList
      } yield redirect.`if`

    val ifElses: List[IfElse] =
      implicitly[LeafExpr[FormTemplate]]
        .exprs(TemplatePath.root, formTemplate)
        .flatMap(_.expr.ifElses)

    val includeIfs: List[IncludeIf] = formTemplate.formKind.allSections.flatMap {
      case Section.NonRepeatingPage(page) => page.includeIf.toList
      case Section.RepeatingPage(page, _) => page.includeIf.toList
      case Section.AddToList(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            includeIf,
            pages,
            repeatsUntil,
            repeatsWhile,
            _,
            _,
            _,
            _,
            _,
            _,
            fields,
            _
          ) =>
        includeIf.toList ++ pages.toList.flatMap(_.includeIf.toList) ++ fields.fold(List.empty[IncludeIf])(
          _.toList.flatMap(_.includeIf.toList)
        ) ++ repeatsUntil.toList ++ repeatsWhile.toList
    } ++ fieldsIncludeIfs ++ destinationsIncludeIfs ++ summarySectionIncludeIfs ++ choiceIncludeIfs ++ miniSummaryListIncludeIfs ++ redirectsIncludeIfs ++ confirmationRedirectsIncludeIfs

    def validate(
      c: String,
      possibleValues: Set[String],
      optionsSize: Int,
      formComponentId: FormComponentId,
      exprString: String,
      componentDescription: String
    ): Either[UnexpectedState, Unit] =
      if (possibleValues.isEmpty) {
        Try(c.toInt) match {
          case Success(index) =>
            val maxIndex = optionsSize - 1
            if (maxIndex < index) {
              Left(
                UnexpectedState(
                  s"Expression '$exprString' has wrong index $c. $componentDescription $formComponentId has only $optionsSize elements. Use index from 0 to $maxIndex"
                )
              )
            } else Right(())
          case Failure(f) =>
            Left(UnexpectedState(s"Expression '$exprString' is invalid. '$c' needs to be a number"))
        }

      } else {
        if (possibleValues(c)) {
          Right(())
        } else {
          val possibleValuesStr = possibleValues.mkString(", ")
          Left(
            UnexpectedState(
              s"Expression '$exprString' is invalid. '$c' is not one of the possible values: $possibleValuesStr"
            )
          )
        }
      }

    def invalidTopLevelBooleanExpr[A](id: BooleanExprId): Either[UnexpectedState, A] = Left(
      UnexpectedState(
        s"Top level 'booleanExpressions' named: ${id.id} is not defined."
      )
    )

    def rewrite(booleanExpr: BooleanExpr): Either[UnexpectedState, BooleanExpr] = booleanExpr match {
      case Not(booleanExpr) => rewrite(booleanExpr).map(Not(_))
      case And(booleanExprL, booleanExprR) =>
        for {
          l <- rewrite(booleanExprL)
          r <- rewrite(booleanExprR)
        } yield And(l, r)
      case Or(booleanExprL, booleanExprR) =>
        for {
          l <- rewrite(booleanExprL)
          r <- rewrite(booleanExprR)
        } yield Or(l, r)
      case be @ Contains(ctx @ FormCtx(formComponentId), Constant(c)) =>
        val exprString = s"$formComponentId contains $c"
        fcLookup
          .get(formComponentId)
          .fold[Either[UnexpectedState, BooleanExpr]](missingFormComponentId(formComponentId)) {
            case Choice(_, options, _, _, _, _, _, _, _, _) =>
              val possibleValues = options.collect { case OptionData.ValueBased(_, value, _) =>
                value
              }.toSet
              validate(c, possibleValues, options.size, formComponentId, exprString, "Choice").map(_ => be)
            case RevealingChoice(options, _) =>
              val possibleValues = options
                .map(_.choice)
                .collect { case OptionData.ValueBased(_, value, _) =>
                  value
                }
                .toSet
              validate(c, possibleValues, options.size, formComponentId, exprString, "Revealing choice").map(_ => be)
            case otherwise => Right(be)
          }
      case be @ EqualsWithConstant(ctx @ FormCtx(formComponentId), Constant(c), swapped) =>
        val exprString = if (swapped) s"$c = $formComponentId" else s"$formComponentId = $c"

        def invalidUsage(component: String): Either[UnexpectedState, BooleanExpr] =
          Left(
            UnexpectedState(
              s"Multivalue $component cannot be used together with '='. Replace '$exprString' with '$formComponentId contains $c' instead."
            )
          )

        val rewriter = Contains(ctx, Constant(c))
        fcLookup
          .get(formComponentId)
          .fold[Either[UnexpectedState, BooleanExpr]](missingFormComponentId(formComponentId)) {
            case Choice(Radio | YesNo, options, _, _, _, _, _, _, _, _) =>
              val possibleValues = options.collect { case OptionData.ValueBased(_, value, _) =>
                value
              }.toSet
              validate(c, possibleValues, options.size, formComponentId, exprString, "Choice").map(_ => rewriter)
            case Choice(Checkbox, _, _, _, _, _, _, _, _, _) => invalidUsage("choice")
            case RevealingChoice(_, true)                    => invalidUsage("revealing choice")
            case RevealingChoice(options, false) =>
              val possibleValues = options
                .map(_.choice)
                .collect { case OptionData.ValueBased(_, value, _) =>
                  value
                }
                .toSet
              validate(c, possibleValues, options.size, formComponentId, exprString, "Revealing choice")
                .map(_ => rewriter)
            case otherwise => Right(be)
          }
      case TopLevelRef(id) => invalidTopLevelBooleanExpr(id)
      case be              => Right(be)
    }

    type Possible[A] = Either[UnexpectedState, A]

    val rewriteIncludeIfRules: Possible[List[(IncludeIf, IncludeIf)]] =
      includeIfs.traverse { includeIf =>
        rewrite(includeIf.booleanExpr)
          .map(booleanExpr => includeIf -> IncludeIf(booleanExpr)): Possible[(IncludeIf, IncludeIf)]
      }
    val rewriteValidIfRules: Possible[List[(ValidIf, ValidIf)]] =
      validIfs.traverse { validIf =>
        rewrite(validIf.booleanExpr)
          .map(booleanExpr => validIf -> ValidIf(booleanExpr)): Possible[(ValidIf, ValidIf)]
      }

    // Updating of FormTemplate with rewritten IfElse expression would be complex, so we are not going to do it.
    // We only need to raise an error when such rewrite is needed.
    // Raising of an error is enough because no invalid IfElse exists in production.
    val rewriteIfElseRules: Possible[List[Unit]] =
      ifElses.traverse { ifElse =>
        rewrite(ifElse.cond)
          .flatMap { booleanExpr =>
            if (booleanExpr == ifElse.cond) {
              booleanExpr match {
                case TopLevelRef(id) => invalidTopLevelBooleanExpr(id)
                case _               => Right(())
              }
            } else
              Left(
                UnexpectedState(
                  "Operator '=' in combination with a choice component cannot be used in if-then-else expression. Use 'contains' operator instead. This is expression triggering this error: " + ifElse.cond
                )
              )
          }: Possible[Unit]
      }

    for {
      includeIfRules <- rewriteIncludeIfRules
      validIfRules   <- rewriteValidIfRules
      _              <- rewriteIfElseRules
    } yield {
      val includeIfRulesLookup: Map[IncludeIf, IncludeIf] = includeIfRules.toMap
      val validIfRulesLookup: Map[ValidIf, ValidIf] = validIfRules.toMap

      def replaceIncludeIf(includeIf: Option[IncludeIf]): Option[IncludeIf] =
        includeIf.flatMap(includeIfRulesLookup.get)

      def replaceValidIf(validIf: Option[ValidIf]): Option[ValidIf] =
        validIf.flatMap(validIfRulesLookup.get)

      def replaceValidator(validator: FormComponentValidator): FormComponentValidator =
        validator.copy(validIf = validIfRulesLookup.get(validator.validIf).getOrElse(validator.validIf))

      def replaceFormComponentNested(formComponent: FormComponent): FormComponent = formComponent match {
        case IsGroup(group) =>
          replaceFormComponent(formComponent).copy(
            `type` = group.copy(fields = group.fields.map(replaceFormComponent))
          )
        case IsRevealingChoice(revealingChoice) =>
          replaceFormComponent(formComponent).copy(
            `type` = revealingChoice.copy(options =
              revealingChoice.options.map(rcElement =>
                rcElement.copy(revealingFields = rcElement.revealingFields.map(replaceFormComponent))
              )
            )
          )
        case IsChoice(choice) =>
          replaceFormComponent(formComponent).copy(
            `type` = choice.copy(options = choice.options.map {
              case OptionData.ValueBased(l, v, i) => OptionData.ValueBased(l, v, replaceIncludeIf(i))
              case OptionData.IndexBased(l, i)    => OptionData.IndexBased(l, replaceIncludeIf(i))
            })
          )
        case otherwise => replaceFormComponent(formComponent)
      }

      def replaceFormComponent(formComponent: FormComponent): FormComponent = formComponent.copy(
        validIf = replaceValidIf(formComponent.validIf),
        includeIf = replaceIncludeIf(formComponent.includeIf),
        validators = formComponent.validators.map(replaceValidator)
      )

      def replaceFields(fields: List[FormComponent]): List[FormComponent] = fields.map(replaceFormComponentNested)

      def replaceFieldsNel(fields: Option[NonEmptyList[FormComponent]]): Option[NonEmptyList[FormComponent]] =
        fields.map(_.map(replaceFormComponentNested))

      def replaceDeclarationSection(declarationSection: DeclarationSection): DeclarationSection =
        declarationSection.copy(fields = replaceFields(declarationSection.fields))

      def replaceSummarySection(summarySection: SummarySection): SummarySection =
        summarySection.copy(
          fields = replaceFieldsNel(summarySection.fields),
          includeIf = replaceIncludeIf(summarySection.includeIf)
        )

      def replaceAcknowledgementSection(acknowledgementSection: AcknowledgementSection): AcknowledgementSection =
        acknowledgementSection.copy(fields = replaceFields(acknowledgementSection.fields))

      def replaceDestinations(destinations: Destinations): Destinations = destinations match {
        case dl: Destinations.DestinationList =>
          dl.copy(
            declarationSection = dl.declarationSection.map(replaceDeclarationSection),
            acknowledgementSection = replaceAcknowledgementSection(dl.acknowledgementSection),
            destinations = dl.destinations.map(replaceDestination)
          )
        case dp: Destinations.DestinationPrint => dp
      }

      def replaceDestination(destination: Destination) =
        destination match {
          case h @ HmrcDms(_, _, _, _, _, includeIf, _, _, _, _, _) =>
            h.copy(includeIf = replaceDesIncludeIf(includeIf))
          case c @ Destination.Composite(_, includeIf, _) => c.copy(includeIf = replaceDesIncludeIf(includeIf))
          case e @ Email(_, _, includeIf, _, _, _)        => e.copy(includeIf = replaceDesIncludeIf(includeIf))
          case h @ HandlebarsHttpApi(_, _, _, _, _, _, includeIf, _) =>
            h.copy(includeIf = replaceDesIncludeIf(includeIf))
          case l @ Log(_)                              => l
          case s @ StateTransition(_, _, includeIf, _) => s.copy(includeIf = replaceDesIncludeIf(includeIf))
          case s @ SubmissionConsolidator(_, _, _, _, includeIf, _) =>
            s.copy(includeIf = replaceDesIncludeIf(includeIf))
        }

      def replaceDesIncludeIf(desIncludeIfValue: DestinationIncludeIf) = desIncludeIfValue match {
        case s @ DestinationIncludeIf.StringValue(_) => s
        case i @ DestinationIncludeIf.IncludeIfValue(includeIf) =>
          i.copy(value = replaceIncludeIf(Some(includeIf)).getOrElse(IncludeIf(IsTrue)))
      }

      def replaceRedirects(redirects: Option[NonEmptyList[RedirectCtx]]) =
        redirects.flatMap(
          _.toList.map(r => r.copy(`if` = replaceIncludeIf(Some(r.`if`)).getOrElse(r.`if`))).toNel
        )

      def replaceConfirmationRedirects(redirects: NonEmptyList[ConfirmationRedirect]) =
        redirects.map(r => r.copy(`if` = replaceIncludeIf(Some(r.`if`)).getOrElse(r.`if`)))

      def replaceConfirmation(confirmation: Option[Confirmation]): Option[Confirmation] =
        confirmation.map(c => c.copy(redirects = replaceConfirmationRedirects(c.redirects)))

      def updateSection(section: Section): Section =
        section match {
          case s: Section.NonRepeatingPage =>
            s.copy(
              page = s.page.copy(
                includeIf = replaceIncludeIf(s.page.includeIf),
                fields = replaceFields(s.page.fields),
                redirects = replaceRedirects(s.page.redirects),
                confirmation = replaceConfirmation(s.page.confirmation)
              )
            )
          case s: Section.RepeatingPage =>
            s.copy(
              page = s.page.copy(
                includeIf = replaceIncludeIf(s.page.includeIf),
                fields = replaceFields(s.page.fields),
                redirects = replaceRedirects(s.page.redirects),
                confirmation = replaceConfirmation(s.page.confirmation)
              )
            )
          case s: Section.AddToList =>
            s.copy(
              includeIf = replaceIncludeIf(s.includeIf),
              repeatsUntil = replaceIncludeIf(s.repeatsUntil),
              repeatsWhile = replaceIncludeIf(s.repeatsWhile),
              pages = s.pages.map(page =>
                page.copy(
                  includeIf = replaceIncludeIf(page.includeIf),
                  fields = replaceFields(page.fields),
                  redirects = replaceRedirects(page.redirects),
                  confirmation = replaceConfirmation(page.confirmation)
                )
              ),
              fields = replaceFieldsNel(s.fields)
            )
        }

      formTemplate.copy(
        formKind = formTemplate.formKind.fold[FormKind](classic =>
          classic.copy(
            sections = classic.sections.map(updateSection)
          )
        )(taskList =>
          taskList.copy(
            sections = taskList.sections.map(taskSection =>
              taskSection.copy(
                tasks = taskSection.tasks.map(task =>
                  task.copy(
                    sections = task.sections.map(updateSection),
                    summarySection = task.summarySection.map(replaceSummarySection)
                  )
                )
              )
            )
          )
        ),
        destinations = replaceDestinations(formTemplate.destinations),
        summarySection = replaceSummarySection(formTemplate.summarySection)
      )
    }
  }
}
