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

package uk.gov.hmrc.gform.submission

import cats.MonadError
import cats.syntax.flatMap._
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId

package object destinations {
  private val logger = LoggerFactory.getLogger(getClass)
  def genericLogMessage(formId: FormId, destinationId: DestinationId, msg: String): String =
    f"${formId.value}%-60s ${destinationId.id}%-30s $msg"

  def raiseDestinationError[T, M[_]](formId: FormId, destinationId: DestinationId, throwable: Throwable)(implicit
    monadError: MonadError[M, Throwable]
  ): M[T] = {
    val fullMsg = genericLogMessage(formId, destinationId, throwable.getMessage)
    monadError.pure(logger.error(fullMsg, throwable)) >>
      monadError.raiseError[T](new Exception(fullMsg, throwable))
  }
}
