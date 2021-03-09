/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.json.{ Format, Json }
import play.api.mvc.Request
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ JsonUtils, QueryParam }

case class QueryParams(params: Map[QueryParam, QueryParamValue]) extends AnyVal {

  def toPlayQueryParams: Map[String, Seq[String]] = params.map { case (param, value) =>
    param.value -> Seq(value.value)
  }

  def apply(queryParam: QueryParam): String = params.get(queryParam).fold("")(_.value)

}

object QueryParams {
  val empty = QueryParams(Map.empty)

  implicit val queryParamMapFormat: Format[Map[QueryParam, QueryParamValue]] =
    JsonUtils.formatMap(QueryParam.apply, _.value)
  implicit val format: Format[QueryParams] = Json.format

  def fromRequest[A](request: Request[A]): QueryParams = QueryParams {
    request.queryString.map { case (key, values) =>
      QueryParam(key) -> QueryParamValue(values.headOption.getOrElse(""))
    }
  }
}
