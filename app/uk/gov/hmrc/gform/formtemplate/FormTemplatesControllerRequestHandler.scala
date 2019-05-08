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

import cats.implicits._
import play.api.libs.json.{ JsObject, Reads }
import play.api.mvc.{ Result, Results }
import uk.gov.hmrc.gform.core.{ FOpt, Opt, fromOptA }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateRaw }
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global

trait RequestHandlerAlg[F[_]] {
  def handleRequest(templateRaw: FormTemplateRaw): F[Unit]
}

class FormTemplatesControllerRequestHandler[F[_]](
  verifyAndSave: FormTemplate => FOpt[Unit],
  save: FormTemplateRaw => FOpt[Unit]) {

  import FormTemplatesControllerRequestHandler._

  val futureInterpreter = new RequestHandlerAlg[FOpt] {
    override def handleRequest(templateRaw: FormTemplateRaw): FOpt[Unit] = {
      val formTemplateOpt: Opt[FormTemplate] =
        implicitly[Reads[FormTemplate]]
          .reads(backwardsCompatibleLanguage(templateRaw.value))
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
  def normaliseJSON(jsonValue: JsValue): JsValue =
    jsonValue match {
      case jsonObj: JsObject =>
        val withFormCategory = ensureFormCategory(jsonObj)
        val withLang = backwardsCompatibleLanguage(withFormCategory)
        withLang
      case _ => jsonValue
    }

  def backwardsCompatibleLanguage(jsonObj: JsObject): JsObject =
    if (jsonObj.keys.contains("languages")) jsonObj
    else jsonObj + ("languages" -> Json.toJson(List("en")))
  def ensureFormCategory(jsonObj: JsObject): JsObject =
    if (jsonObj.keys.contains("formCategory")) jsonObj
    else jsonObj + ("formCategory" -> Json.toJson("default"))
}
