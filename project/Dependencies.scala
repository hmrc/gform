import play.core.PlayVersion
import sbt._
object Dependencies {

  val appName = "gform"

  lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"
  val handlebarsVersion = "4.2.0"
  val hmrcMongoVersion = "0.50.0"

  val compile = Seq(
    "com.github.pureconfig"      %% "pureconfig"                % "0.16.0",
    "uk.gov.hmrc.mongo"          %% "hmrc-mongo-play-28"        % hmrcMongoVersion,
    "uk.gov.hmrc"                %% "http-caching-client"       % "9.5.0-play-28",
    "uk.gov.hmrc"                %% "bootstrap-backend-play-28" % "5.12.0",
    "uk.gov.hmrc"                %% "domain"                    % "6.2.0-play-28",
    "org.scala-lang.modules"     %% "scala-parser-combinators"  % "2.1.0",
    "org.julienrf"               %% "play-json-derived-codecs"  % "10.0.2",
    "org.typelevel"              %% "cats-core"                 % "1.6.0",
    "org.typelevel"              %% "case-insensitive"          % "0.3.0",
    "com.openhtmltopdf"           % "openhtmltopdf-pdfbox"      % "1.0.6",
    "com.chuusai"                %% "shapeless"                 % "2.3.3",
    "org.scala-graph"            %% "graph-core"                % "1.12.5",
    "com.github.jknack"           % "handlebars"                % handlebarsVersion,
    "com.github.jknack"           % "handlebars-jackson2"       % handlebarsVersion,
    "com.softwaremill.quicklens" %% "quicklens"                 % "1.4.11",
    "uk.gov.service.notify"       % "notifications-java-client" % "3.17.0-RELEASE",
    "org.apache.commons"          % "commons-text"              % "1.9"
  )

  def test(scope: String = "test,it") = Seq(
    "uk.gov.hmrc"            %% "service-integration-test"    % "1.1.0-play-28"     % scope,
    "org.scalatestplus"      %% "scalacheck-1-14"             % "3.1.1.0"           % scope,
    "org.pegdown"             % "pegdown"                     % "1.6.0"             % scope,
    "com.typesafe.play"      %% "play-test"                   % PlayVersion.current % scope,
    "org.scalamock"          %% "scalamock-scalatest-support" % "3.6.0"             % scope,
    "org.scalatestplus.play" %% "scalatestplus-play"          % "5.1.0"             % scope,
    "org.jsoup"               % "jsoup"                       % "1.11.3"            % scope,
    "com.github.tomakehurst"  % "wiremock-jre8"               % "2.27.1"            % scope,
    "org.scalameta"          %% "munit"                       % "0.7.22"            % scope,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-28"     % hmrcMongoVersion    % scope,
    "com.storm-enroute"      %% "scalameter"                  % "0.19"              % scope,
    "com.typesafe"           %% "ssl-config-core"             % "0.4.3"             % scope // # Needed for it:test to run on JDK 17
  )
}
