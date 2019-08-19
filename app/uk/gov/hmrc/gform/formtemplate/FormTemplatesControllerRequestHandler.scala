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

import cats.instances.future._
import cats.syntax.either._
import play.api.libs.json.{ Format, JsObject, JsResult, JsString, JsValue, Json, Reads, __ }
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import scala.concurrent.ExecutionContext
import scala.language.postfixOps
import uk.gov.hmrc.gform.core.{ FOpt, Opt, fromOptA }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateRaw }

trait RequestHandlerAlg[F[_]] {
  def handleRequest(templateRaw: FormTemplateRaw): F[Unit]
}

class FormTemplatesControllerRequestHandler[F[_]](
  verifyAndSave: FormTemplate => FOpt[Unit],
  save: FormTemplateRaw => FOpt[Unit])(implicit ec: ExecutionContext) {

  import FormTemplatesControllerRequestHandler._

  val futureInterpreter = new RequestHandlerAlg[FOpt] {
    override def handleRequest(templateRaw: FormTemplateRaw): FOpt[Unit] = {

      val formTemplateOpt: Opt[FormTemplate] = FormTemplate
        .transformAndReads(templateRaw.value)
        .fold(errors => UnexpectedState(errors.toString()).asLeft, valid => valid.asRight)

      processAndPersistTemplate(formTemplateOpt, templateRaw)
    }
  }

  private def processAndPersistTemplate(formTemplateOpt: Opt[FormTemplate], templateRaw: FormTemplateRaw): FOpt[Unit] =
    for {
      ft <- fromOptA(formTemplateOpt)
      _  <- verifyAndSave(ft)
      _  <- save(templateRaw)
    } yield ()
}

object FormTemplatesControllerRequestHandler {

  def normaliseJSON(jsonValue: JsValue): JsResult[JsObject] = {

    val drmValue = (__ \ 'draftRetrievalMethod \ 'value).json
      .copyFrom((__ \ 'draftRetrievalMethod).json.pick orElse Reads.pure(JsString("onePerUser")))

    val drmShowContinueOrDeletePage = (__ \ 'draftRetrievalMethod \ 'showContinueOrDeletePage).json
      .copyFrom((__ \ 'showContinueOrDeletePage).json.pick orElse Reads.pure(JsString("true")))

    val pruneShowContinueOrDeletePage = (__ \ 'showContinueOrDeletePage).json.prune

    val ensureFormCategory = (__ \ 'formCategory).json
      .copyFrom((__ \ 'formCategory).json.pick orElse Reads.pure(JsString("default")))

    val ensureLanguages = (__ \ 'languages).json
      .copyFrom((__ \ 'languages).json.pick orElse Reads.pure(Json.arr("en")))

    val ensureParentFormSubmissionRefs = (__ \ 'parentFormSubmissionRefs).json
      .copyFrom((__ \ 'parentFormSubmissionRefs).json.pick orElse Reads.pure(Json.arr()))

    val transformer: Reads[JsObject] =
      pruneShowContinueOrDeletePage and drmValue and drmShowContinueOrDeletePage and ensureFormCategory and ensureLanguages and ensureParentFormSubmissionRefs reduce

    jsonValue.transform(transformer)
  }
}
