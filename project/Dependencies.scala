import play.core.PlayVersion
import sbt._
object Dependencies {

  val appName = "gform"

  lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"
  val handlebarsVersion = "4.2.0"
  val hmrcMongoVersion = "0.74.0"
  val bootstrapVersion = "7.13.0"

  val compile = Seq(
    "com.github.pureconfig"      %% "pureconfig"                        % "0.16.0",
    "uk.gov.hmrc.mongo"          %% "hmrc-mongo-play-28"                % hmrcMongoVersion,
    "uk.gov.hmrc.mongo"          %% "hmrc-mongo-work-item-repo-play-28" % hmrcMongoVersion,
    "uk.gov.hmrc"                %% "http-caching-client"               % "10.0.0-play-28",
    "uk.gov.hmrc"                %% "bootstrap-backend-play-28"         % bootstrapVersion,
    "uk.gov.hmrc"                %% "domain"                            % "8.1.0-play-28",
    "org.scala-lang.modules"     %% "scala-parser-combinators"          % "1.1.2",
    "org.julienrf"               %% "play-json-derived-codecs"          % "10.0.2",
    "org.typelevel"              %% "cats-core"                         % "2.8.0",
    "org.typelevel"              %% "case-insensitive"                  % "0.3.0",
    "com.openhtmltopdf"           % "openhtmltopdf-pdfbox"              % "1.0.6",
    "com.chuusai"                %% "shapeless"                         % "2.3.3",
    "org.scala-graph"            %% "graph-core"                        % "1.12.5",
    "com.github.jknack"           % "handlebars"                        % handlebarsVersion,
    "com.github.jknack"           % "handlebars-jackson2"               % handlebarsVersion,
    "com.softwaremill.quicklens" %% "quicklens"                         % "1.9.0",
    "uk.gov.service.notify"       % "notifications-java-client"         % "3.17.3-RELEASE",
    "org.apache.commons"          % "commons-text"                      % "1.9",
    "org.scala-graph"            %% "graph-core"                        % "1.13.1",
    "uk.gov.hmrc.objectstore"    %% "object-store-client-play-28"       % "1.0.0",
    "io.circe"                   %% "circe-core"                        % "0.14.3",
    "io.circe"                   %% "circe-generic"                     % "0.14.3",
    "io.circe"                   %% "circe-parser"                      % "0.14.3",
    "io.circe"                   %% "circe-literal"                     % "0.14.3",
    "com.github.tototoshi"       %% "scala-csv"                         % "1.3.10",
    "org.json4s"                 %% "json4s-core"                       % "3.2.11",
    "org.json4s"                 %% "json4s-native"                     % "3.2.11"
  )

  def test(scope: String = "test,it") = Seq(
    "uk.gov.hmrc"            %% "service-integration-test"    % "1.3.0-play-28"     % scope,
    "uk.gov.hmrc"            %% "bootstrap-test-play-28"      % bootstrapVersion    % scope,
    "org.scalatestplus"      %% "scalacheck-1-14"             % "3.1.1.0"           % scope,
    "org.pegdown"             % "pegdown"                     % "1.6.0"             % scope,
    "com.typesafe.play"      %% "play-test"                   % PlayVersion.current % scope,
//    "org.scalamock"          %% "scalamock-scalatest-support" % "3.6.0"             % scope,
    "org.scalamock" %% "scalamock" % "5.2.0" % scope,
    "org.scalatest" %% "scalatest" % "3.2.0" % scope,
    "org.scalatestplus.play" %% "scalatestplus-play"          % "5.1.0"             % scope,
    "org.jsoup"               % "jsoup"                       % "1.11.3"            % scope,
    "com.github.tomakehurst"  % "wiremock-jre8"               % "2.27.1"            % scope,
    "org.scalameta"          %% "munit"                       % "0.7.22"            % scope,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-28"     % hmrcMongoVersion    % scope,
    "com.storm-enroute"      %% "scalameter"                  % "0.19"              % scope,
    "com.typesafe"           %% "ssl-config-core"             % "0.4.3"             % scope // # Needed for it:test to run on JDK 17
  )
}
