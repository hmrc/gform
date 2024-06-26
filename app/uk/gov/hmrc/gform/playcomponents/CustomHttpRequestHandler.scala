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

package uk.gov.hmrc.gform.playcomponents

import play.api.http.{ DefaultHttpRequestHandler, HttpConfiguration, HttpErrorHandler }
import play.api.mvc.{ EssentialFilter, Handler, RequestHeader }
import play.api.routing.Router
import play.api.http.DefaultHttpFilters
import play.api.OptionalDevContext
import play.core.DefaultWebCommands
import javax.inject.Provider

class CustomHttpRequestHandler(
  router: Router,
  httpErrorHandler: HttpErrorHandler,
  httpConfiguration: HttpConfiguration,
  httpFilters: Seq[EssentialFilter]
) extends DefaultHttpRequestHandler(
      webCommands = new DefaultWebCommands,
      optDevContext = new OptionalDevContext(None),
      router = new Provider[Router] { def get = router },
      errorHandler = httpErrorHandler,
      configuration = httpConfiguration,
      filters = new DefaultHttpFilters(httpFilters: _*)
    ) {
  override def routeRequest(request: RequestHeader): Option[Handler] =
    router
      .handlerFor(request)
      .orElse {
        CustomHttpRequestHandler
          .dropTrailingSlash(request)
          .flatMap(router.handlerFor)
      }
}

object CustomHttpRequestHandler {
  private def dropTrailingSlash(request: RequestHeader): Option[RequestHeader] =
    Some(request.target.path)
      .filter(_.endsWith("/"))
      .map(p => request.withTarget(request.target.withPath(p.dropRight(1))))
}
