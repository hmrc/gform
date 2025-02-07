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

package uk.gov.hmrc.gform.submission.destinations

import cats.{ ApplicativeError, MonadError }
import cats.instances.list._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.show._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import uk.gov.hmrc.gform.notifier.{ NotifierEmail, NotifierEmailReference }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId }
import uk.gov.hmrc.gform.sharedmodel.notifier.{ NotifierEmailAddress, NotifierTemplateId }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue

object NotifierEmailBuilder {
  def apply[M[_]](
    templateId: NotifierTemplateId,
    d: Destination.Email,
    structuredFormData: StructuredFormValue.ObjectStructure
  )(implicit
    M: MonadError[M, Throwable]
  ): M[NotifierEmail] =
    for {
      to              <- extractTo(d, structuredFormData)
      personalisation <- extractPersonalisation(d, structuredFormData)
    } yield NotifierEmail(templateId, to, personalisation, NotifierEmailReference(""))

  private def extractTo[M[_]](d: Destination.Email, structuredFormData: StructuredFormValue.ObjectStructure)(implicit
    M: MonadError[M, Throwable]
  ): M[NotifierEmailAddress] =
    extractText(d.id, structuredFormData, d.to)
      .map(NotifierEmailAddress(_))

  private def extractPersonalisation[M[_]](
    d: Destination.Email,
    structuredFormData: StructuredFormValue.ObjectStructure
  )(implicit M: ApplicativeError[M, Throwable]): M[Map[String, String]] =
    d.personalisation.toList
      .traverse { case (k, v) =>
        extractText(d.id, structuredFormData, v).map(value => (k.value, value))
      }
      .map(_.toMap)

  private def extractText[M[_]](
    destinationId: DestinationId,
    structuredFormData: StructuredFormValue.ObjectStructure,
    formComponentId: FormComponentId
  )(implicit M: ApplicativeError[M, Throwable]): M[String] =
    structuredFormData.fields
      .find(_.name.name === formComponentId.value)
      .map(_.value)
      .map[M[String]] {
        case StructuredFormValue.TextNode(value) => value.pure[M]
        case StructuredFormValue.ObjectStructure(_) =>
          M.raiseError(new Exception(unexpectedObject(destinationId, formComponentId)))
        case StructuredFormValue.ArrayNode(_) =>
          M.raiseError(new Exception(unexpectedArray(destinationId, formComponentId)))
      }
      .getOrElse(M.raiseError(new Exception(missingField(destinationId, formComponentId))))

  def unexpectedObject(destinationId: DestinationId, formComponentId: FormComponentId): String =
    show"NotifierEmailBuilder: Expected $formComponentId to be a simple text value. Got an Object. Destination id = $destinationId."

  def unexpectedArray(destinationId: DestinationId, formComponentId: FormComponentId): String =
    show"NotifierEmailBuilder: Expected $formComponentId to be a simple text value. Got an Array. Destination id = $destinationId."

  def missingField(destinationId: DestinationId, formComponentId: FormComponentId): String =
    show"NotifierEmailBuilder: Failed to extract '$formComponentId' for destination id $destinationId. No such form component."
}
