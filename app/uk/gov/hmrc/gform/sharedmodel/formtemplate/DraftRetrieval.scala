/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.implicits.catsSyntaxEq
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup

case class DraftRetrieval(mapping: Map[AffinityGroup, DraftRetrievalMethod])

object DraftRetrieval {
  val readsDraftRetrievalMethod: Reads[DraftRetrievalMethod] = Reads { json =>
    (json \ "method", json \ "showContinueOrDeletePage") match {
      case (JsDefined(JsString(field)), JsDefined(JsBoolean(bool))) =>
        DraftRetrievalMethod.Helper(field, bool).toDraftRetrieval
      case (JsDefined(JsString(field)), _) =>
        if (field === "submissionReference") {
          DraftRetrievalMethod.Helper(field, true).toDraftRetrieval
        } else {
          DraftRetrievalMethod.Helper(field, false).toDraftRetrieval
        }
      case (method, showContinueOrDeletePage) =>
        JsError(
          s"Failure, $method and $showContinueOrDeletePage are invalid in combination with DraftRetrieval"
        )
    }
  }

  implicit val formatDraftRetrievalMethod: OFormat[DraftRetrievalMethod] = OFormatWithTemplateReadFallback(
    readsDraftRetrievalMethod
  )

  def reads: Reads[DraftRetrieval] = Reads { json =>
    for {
      a <- (json \ "agent").validateOpt[DraftRetrievalMethod].map(_.map(AffinityGroup.Agent -> _))
      i <- (json \ "individual").validateOpt[DraftRetrievalMethod].map(_.map(AffinityGroup.Individual -> _))
      o <- (json \ "organisation").validateOpt[DraftRetrievalMethod].map(_.map(AffinityGroup.Organisation -> _))
    } yield DraftRetrieval(mapping = (a ++ i ++ o).toMap)
  }

  val writes: Writes[DraftRetrieval] = Writes { draftRetrieval =>
    Json
      .obj(
        "agent"        -> draftRetrieval.mapping.get(AffinityGroup.Agent),
        "individual"   -> draftRetrieval.mapping.get(AffinityGroup.Individual),
        "organisation" -> draftRetrieval.mapping.get(AffinityGroup.Organisation)
      )
      .fields
      .filterNot { case (_, value) => value == JsNull }
      .foldLeft(Json.obj()) { case (acc, (key, value)) =>
        acc + (key -> value)
      }
  }

  implicit val format: Format[DraftRetrieval] = Format[DraftRetrieval](reads, writes)
}
