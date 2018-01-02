/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import java.nio.file.{ Files, Paths }

import collection.JavaConverters._
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.wshttp.{ TestWSHttpIT, WSHttp }
import uk.gov.hmrc.http.HeaderCarrier

import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import scala.util.{ Failure, Success }

class TemplatesInSampleDataSpec extends support.ITSpec {

  //   "Test that templates in sample data can be uploaded" in eventually {
  //     TemplatesInSampleDataSpec.templates foreach { templateFile =>
  //        val template: JsValue = Json.parse(Files.newInputStream(templateFile))
  //       println(template)
  //        gformConnector.upsertTemplate(template).futureValue shouldBe (()) withClue templateFile.toString
  //      }
  //    }
}

object TemplatesInSampleDataSpec {
  lazy val templates = Files
    .newDirectoryStream(Paths.get("sample-data"))
    .asScala
    .filter(Files.isRegularFile(_))
    .filter(_.getFileName.toString.matches("template-aaa(.*)json"))
    .toList
}

object UploadAllTemplates extends App {

  val wsclient = TestWSHttpIT
  val baseUrl = """http://localhost:9196/gform"""
  val gformConnector = new GformConnector(wsclient, baseUrl)
  implicit val hc = HeaderCarrier()

  TemplatesInSampleDataSpec.templates foreach { templateFile =>
    println(s"uploading $templateFile")
    val template: JsValue = Json.parse(Files.newInputStream(templateFile))
    gformConnector.upsertTemplate(template).onComplete {
      case Failure(f) => println(f)
      case Success(_) => ()
    }
  }
}
