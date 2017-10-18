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
  val microserviceBootstrapVersion = "5.16.0"
  val domainVersion = "5.0.0"
  val hmrcTestVersion = "3.0.0"
  val httpCachingClientVersion = "6.3.0"
  val playConfigVersion = "4.3.0"
  val playAuthorisationVersion = "4.3.0"
  val playHealthVersion = "2.1.0"
  val playReactivemongoVersion = "5.2.0"
  val playUrlBindersVersion = "2.1.0"
  val playJsonLoggerVersion = "3.1.0"

  val catsVersion = "0.9.0"
  val playAuditing = "2.10.0"
  val pdfboxVersion = "2.0.7"
  val spdfVersion = "1.4.0"
  val shapelessVersion = "2.3.2"
  val pureConfigVersion = "0.7.2"
  val simulacrumVersion = "0.11.0"

  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "play-reactivemongo" % playReactivemongoVersion,
    "com.github.pureconfig" %% "pureconfig" % "0.8.0",
    "uk.gov.hmrc" %% "http-caching-client" % httpCachingClientVersion,
    "uk.gov.hmrc" %% "microservice-bootstrap" % microserviceBootstrapVersion,
    "uk.gov.hmrc" %% "play-authorisation" % playAuthorisationVersion,
    "uk.gov.hmrc" %% "play-health" % playHealthVersion,
    "uk.gov.hmrc" %% "play-url-binders" % playUrlBindersVersion,
    "uk.gov.hmrc" %% "play-config" % playConfigVersion,
    "uk.gov.hmrc" %% "play-auditing" % playAuditing,
    "uk.gov.hmrc" %% "logback-json-logger" % playJsonLoggerVersion,
    "uk.gov.hmrc" %% "domain" % domainVersion,
    "com.codecommit" %% "parseback-core" % parsebackVersion,
    "com.codecommit" %% "parseback-cats" % parsebackVersion,
    "org.julienrf" %% "play-json-derived-codecs" % "3.3",
    "org.typelevel" %% "cats" % catsVersion,
    "org.apache.pdfbox" % "pdfbox" % pdfboxVersion,
    "io.github.cloudify" %% "spdf" % spdfVersion,
    "com.chuusai" %% "shapeless" % shapelessVersion,
    "com.github.pureconfig" %% "pureconfig" % pureConfigVersion,
    "com.github.mpilquist" %% "simulacrum" % simulacrumVersion
  )


  def test(scope: String = "test,it") = Seq(
    "uk.gov.hmrc" %% "hmrctest" % hmrcTestVersion % scope,
    "org.scalatest" %% "scalatest" % "3.0.4" % scope,
    "org.pegdown" % "pegdown" % "1.6.0" % scope,
    "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % scope,
    "uk.gov.hmrc" %% "reactivemongo-test" % "2.0.0" % scope,
    "org.jsoup" % "jsoup" % "1.10.3" % scope
  )

}
