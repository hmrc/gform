import sbt._
import play.sbt.PlayImport._
import play.core.PlayVersion
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning

object MicroServiceBuild extends Build with MicroService {

  val appName = "gform"

  override lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"
  val microserviceBootstrapVersion = "5.15.0"
  val domainVersion = "4.1.0"
  val hmrcTestVersion = "2.3.0"
  val playConfigVersion = "4.3.0"
  val playAuthorisationVersion = "4.3.0"
  val playHealthVersion = "2.1.0"
  val playReactivemongoVersion = "5.2.0"
  val playUrlBindersVersion = "2.1.0"
  val playJsonLoggerVersion = "3.1.0"

  val catsVersion = "0.9.0"
  val playAuditing = "2.9.0"
  val pdfboxVersion = "2.0.6"
  val spdfVersion = "1.4.0"

  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "play-reactivemongo" % "5.1.0",
    "uk.gov.hmrc" %% "microservice-bootstrap" % "5.8.0",
    "uk.gov.hmrc" %% "play-authorisation" % "4.2.0",
    "uk.gov.hmrc" %% "play-health" % "2.0.0",
    "uk.gov.hmrc" %% "play-url-binders" % "2.0.0",
    "uk.gov.hmrc" %% "play-config" % "3.1.0",
    "uk.gov.hmrc" %% "logback-json-logger" % playJsonLoggerVersion,
    "uk.gov.hmrc" %% "domain" % "4.0.0",
    "com.codecommit" %% "parseback-core" % parsebackVersion,
    "com.codecommit" %% "parseback-cats" % parsebackVersion,
    "org.julienrf" %% "play-json-derived-codecs" % "3.3",
    "org.typelevel" %% "cats" % catsVersion,
    "org.apache.pdfbox" % "pdfbox" % "2.0.4",
    "io.github.cloudify" %% "spdf" % spdfVersion
  )

  def test(scope: String = "test,it") = Seq(
    "uk.gov.hmrc" %% "hmrctest" % hmrcTestVersion % scope,
    "org.scalatest" %% "scalatest" % "3.0.3" % scope,
    "org.pegdown" % "pegdown" % "1.6.0" % scope,
    "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.4.2" % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.0" % scope,
    "uk.gov.hmrc" %% "reactivemongo-test" % "1.6.0" % scope
  )

}
