/*
 * Copyright 2019 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ComponentType, FormTemplate }
import cats.implicits._
import FormTemplateValidator._

import scala.concurrent.ExecutionContext

trait Verifier {
  def verify(formTemplate: FormTemplate)(implicit ec: ExecutionContext): FOpt[Unit] = {

    val sections = formTemplate.sections

    val exprs: List[ComponentType] = sections.flatMap(_.fields.map(_.`type`))

    for {
      _ <- fromOptA(FormTemplateValidator.validateRepeatingSectionFields(sections).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceHelpText(sections).toEither)
      _ <- fromOptA(FormTemplateValidator.validateUniqueFields(sections).toEither)
      _ <- fromOptA(FormTemplateValidator.validateUniqueDestinationIds(formTemplate.destinations).toEither)
      _ <- fromOptA(FormTemplateValidator.validateOneOrMoreHmrcDmsDestination(formTemplate.destinations).toEither)
      _ <- fromOptA(FormTemplateValidator.validateForwardReference(sections).toEither)
      _ <- fromOptA(FormTemplateValidator.validate(exprs, formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateDependencyGraph(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateEnrolmentSection(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateRegimeId(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateEmailParameter(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateEnrolmentIdentifier(formTemplate, userContextComponentType).toEither)
    } yield ()
  }
}
