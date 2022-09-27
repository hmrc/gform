/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.objectstore

import _root_.play.api.libs.streams.Accumulator
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import akka.util.ByteString
import org.slf4j.LoggerFactory
import play.api.mvc._
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.objectstore.client.Path
import uk.gov.hmrc.objectstore.client.config.ObjectStoreClientConfig
import uk.gov.hmrc.objectstore.client.play.PlayObjectStoreClientEither
import uk.gov.hmrc.objectstore.client.play.Implicits._

import scala.concurrent.ExecutionContext

class ObjectStoreController(
  cc: ControllerComponents,
  objectStoreClient: PlayObjectStoreClientEither,
  objectStoreClientConfig: ObjectStoreClientConfig
)(implicit
  val ec: ExecutionContext,
  actorSystem: ActorSystem
) extends BaseController(cc) {

  private val logger = LoggerFactory.getLogger(getClass)

  private def directory(folderName: String): Path.Directory =
    Path.Directory(s"envelopes/$folderName")

  private val streaming: BodyParser[Source[ByteString, _]] = BodyParser { _ =>
    Accumulator.source[ByteString].map(Right.apply)
  }

  def putObject(folderName: String, fileName: String): Action[Source[ByteString, _]] = Action.async(streaming) {
    implicit request =>
      objectStoreClient
        .putObject(directory(folderName).file(fileName), request.body)
        .map(_ => Created("Document stored."))
        .recover {
          case UpstreamErrorResponse(message, statusCode, _, _) =>
            logger.error(s"Upstream error with status code '$statusCode' and message: $message")
            InternalServerError("Upstream error encountered")
          case e: Exception =>
            logger.error(s"An error was encountered saving the document.", e)
            InternalServerError("Error saving the document")
        }
  }
}
