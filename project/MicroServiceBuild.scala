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
  val handlebarsVersion = "4.1.2"
  
  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "play-reactivemongo" % "6.2.0",
    "com.github.pureconfig" %% "pureconfig" % "0.9.2",
    "uk.gov.hmrc" %% "http-caching-client" % "7.1.0",
    "uk.gov.hmrc" %% "microservice-bootstrap" % "8.5.0",
    "uk.gov.hmrc" %% "domain" % "5.2.0",
    "com.codecommit" %% "parseback-core" % parsebackVersion,
    "com.codecommit" %% "parseback-cats" % parsebackVersion,
    "org.julienrf" %% "play-json-derived-codecs" % "3.3",
    "org.typelevel" %% "cats-core" % "1.4.0",
    "org.apache.pdfbox" % "pdfbox" % "2.0.12",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "com.github.mpilquist" %% "simulacrum" % "0.13.0",
    "uk.gov.hmrc" %% "auth-client" % "2.16.0-play-25", // we need AffinityGroup type to stay in sync with frontend
    "org.scala-graph" %% "graph-core" % "1.12.5",
    "com.github.jknack" % "handlebars" % handlebarsVersion,
    "com.github.jknack" % "handlebars-jackson2" % handlebarsVersion,
    "io.monix" %% "monix" % "3.0.0-8084549"
  )


  def test(scope: String = "test,it") = Seq(
    "uk.gov.hmrc" %% "hmrctest" % "3.2.0" % scope,
    "org.scalatest" %% "scalatest" % "3.0.5" % scope,
    "org.scalacheck" %% "scalacheck" % "1.14.0" % scope,
    "org.pegdown" % "pegdown" % "1.6.0" % scope,
    "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % scope,
    "uk.gov.hmrc" %% "reactivemongo-test" % "3.1.0" % scope,
    "org.jsoup" % "jsoup" % "1.11.3" % scope
  )

}
