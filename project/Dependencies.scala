import play.core.PlayVersion
import play.sbt.PlayImport.ws
import sbt._
object Dependencies {

  val appName = "gform"

  lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"
  val handlebarsVersion = "4.1.2"

  val compile = Seq(
    ws,
    "com.github.pureconfig" %% "pureconfig" % "0.10.2",
    "uk.gov.hmrc" %% "simple-reactivemongo" % "7.20.0-play-26",
    "uk.gov.hmrc" %% "http-caching-client" % "9.0.0-play-26",
    "uk.gov.hmrc" %% "bootstrap-play-26" % "1.1.0",
    "uk.gov.hmrc" %% "domain" % "5.3.0",
    "uk.gov.hmrc" %% "auth-client" % "2.27.0-play-26", // we need AffinityGroup type to stay in sync with frontend
    "com.typesafe.play" %% "play-json" % "2.6.13",
    "com.codecommit" %% "parseback-core" % parsebackVersion,
    "com.codecommit" %% "parseback-cats" % parsebackVersion,
    "org.julienrf" %% "play-json-derived-codecs" % "3.3", //upgrading this has caused play-json comparability issue
    "org.typelevel" %% "cats-core" % "1.6.0",
    "org.apache.pdfbox" % "pdfbox" % "2.0.13",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "com.github.mpilquist" %% "simulacrum" % "0.15.0",
    "org.scala-graph" %% "graph-core" % "1.12.5",
    "com.github.jknack" % "handlebars" % handlebarsVersion,
    "com.github.jknack" % "handlebars-jackson2" % handlebarsVersion,
    "com.softwaremill.quicklens" %% "quicklens" % "1.4.11",
    "uk.gov.service.notify" % "notifications-java-client" % "3.14.2-RELEASE"
  )


  def test(scope: String = "test,it") = Seq(
    "uk.gov.hmrc" %% "hmrctest" % "3.5.0-play-25" % scope,
    "org.scalatest" %% "scalatest" % "3.0.5" % scope,
    "org.scalacheck" %% "scalacheck" % "1.14.0" % scope,
    "org.pegdown" % "pegdown" % "1.6.0" % scope,
    "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % scope,
    "org.jsoup" % "jsoup" % "1.11.3" % scope
  )
}
  