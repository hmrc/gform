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

package uk.gov.hmrc.gform.formtemplate

import uk.gov.hmrc.gform.core.{ FOpt, fromOptA }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import cats.implicits._
import scala.util.{ Failure, Success, Try }

trait Rewriter {
  def rewrite(formTemplate: FormTemplate): FOpt[FormTemplate] = fromOptA(validateAndRewriteIncludeIf(formTemplate))

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

  private def lookupFromPage(page: Page): Map[FormComponentId, ComponentType] =
    page.fields.foldLeft(Map.empty[FormComponentId, ComponentType])((acc, fc) => mkComponentTypeLookup(fc) ++ acc)

  private def missingFormComponentId[A](formComponentId: FormComponentId): Either[UnexpectedState, A] =
    Left(UnexpectedState(s"Missing component with id $formComponentId"))

  private def validateAndRewriteIncludeIf(formTemplate: FormTemplate): Either[UnexpectedState, FormTemplate] = {
    val fcLookup: Map[FormComponentId, ComponentType] =
      formTemplate.sections.foldLeft(Map.empty[FormComponentId, ComponentType]) {
        case (acc, Section.NonRepeatingPage(page))            => acc ++ lookupFromPage(page)
        case (acc, Section.RepeatingPage(page, _))            => acc ++ lookupFromPage(page)
        case (acc, Section.AddToList(_, _, _, _, _, _, _, _)) => acc
      }

    val includeIfs: List[IncludeIf] = formTemplate.sections.flatMap {
      case Section.NonRepeatingPage(page)                    => page.includeIf
      case Section.RepeatingPage(page, _)                    => page.includeIf
      case Section.AddToList(_, _, _, includeIf, _, _, _, _) => includeIf
    }

    def validate(
      c: String,
      optionsSize: Int,
      formComponentId: FormComponentId,
      exprString: String,
      componentDescription: String
    ): Either[UnexpectedState, Unit] =
      Try(c.toInt) match {
        case Success(index) =>
          val maxIndex = optionsSize - 1
          if (maxIndex < index) {
            Left(UnexpectedState(
              s"Expression '$exprString' has wrong index $c. $componentDescription $formComponentId has only $optionsSize elements. Use index from 0 to $maxIndex"))
          } else Right(())
        case Failure(f) =>
          Left(UnexpectedState(s"Expression '$exprString' is invalid. '$c' needs to be a number"))
      }

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
            case Choice(_, options, _, _, _) =>
              validate(c, options.size, formComponentId, exprString, "Choice").map(_ => be)
            case RevealingChoice(options, _) =>
              validate(c, options.size, formComponentId, exprString, "Revealing choice").map(_ => be)
            case otherwise => Right(be)
          }
      case be @ EqualsWithConstant(ctx @ FormCtx(formComponentId), Constant(c), swapped) =>
        val exprString = if (swapped) s"$c = $formComponentId" else s"$formComponentId = $c"

        def invalidUsage(component: String): Either[UnexpectedState, BooleanExpr] =
          Left(UnexpectedState(
            s"Multivalue $component cannot be used together with '='. Replace '$exprString' with '$formComponentId contains $c' instead."))

        val rewriter = Contains(ctx, Constant(c))
        fcLookup
          .get(formComponentId)
          .fold[Either[UnexpectedState, BooleanExpr]](missingFormComponentId(formComponentId)) {
            case Choice(Radio | YesNo, options, _, _, _) =>
              validate(c, options.size, formComponentId, exprString, "Choice").map(_ => rewriter)
            case Choice(Checkbox, _, _, _, _) => invalidUsage("choice")
            case RevealingChoice(_, true)     => invalidUsage("revealing choice")
            case RevealingChoice(options, false) =>
              validate(c, options.size, formComponentId, exprString, "Revealing choice").map(_ => rewriter)
            case otherwise => Right(be)
          }
      case be => Right(be)
    }

    type Possible[A] = Either[UnexpectedState, A]

    val rewriteRules: Possible[List[(IncludeIf, IncludeIf)]] =
      includeIfs.traverse { includeIf =>
        rewrite(includeIf.booleanExpr)
          .map(booleanExpr => includeIf -> IncludeIf(booleanExpr)): Possible[(IncludeIf, IncludeIf)]
      }

    rewriteRules.map { rules =>
      val rulesLookup = rules.toMap

      def replace(includeIf: Option[IncludeIf]): Option[IncludeIf] =
        includeIf.fold[Option[IncludeIf]](None)(rulesLookup.get)

      formTemplate.copy(
        sections = formTemplate.sections.map {
          case s: Section.NonRepeatingPage => s.copy(page = s.page.copy(includeIf = replace(s.page.includeIf)))
          case s: Section.RepeatingPage    => s.copy(page = s.page.copy(includeIf = replace(s.page.includeIf)))
          case s: Section.AddToList        => s.copy(includeIf = replace(s.includeIf))
        }
      )
    }
  }
}
