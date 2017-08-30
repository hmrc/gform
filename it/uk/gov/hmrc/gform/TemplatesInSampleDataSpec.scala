package uk.gov.hmrc.gform

import java.nio.file.{Files, Paths}

import collection.JavaConverters._
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.wshttp.{TestWSHttpIT, WSHttp}
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class TemplatesInSampleDataSpec extends support.ITSpec {

   "Test that templates in sample data can be uploaded" in eventually {
     TemplatesInSampleDataSpec.templates foreach { templateFile =>
        val template: JsValue = Json.parse(Files.newInputStream(templateFile))
        gformConnector.upsertTemplate(template).futureValue shouldBe (()) withClue templateFile.toString
      }
    }
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
    gformConnector.upsertTemplate(template).onComplete{
      case Failure(f) => println(f)
      case Success(_) => ()
    }
  }
}
