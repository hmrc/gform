import Dependencies.appDependencies
import org.scalafmt.sbt.ScalafmtPlugin
import play.sbt.PlayImport.PlayKeys
import play.sbt.routes.RoutesKeys.routesImport
import sbt.Keys.{ resolvers, _ }
import sbt._
import uk.gov.hmrc.DefaultBuildSettings.{ addTestReportOption, defaultSettings, scalaSettings }
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion

lazy val scoverageSettings = {
  import scoverage.ScoverageKeys
  Seq(
    ScoverageKeys.coverageExcludedPackages := """uk.gov.hmrc.BuildInfo;._.Routes;._.RoutesPrefix;._Filters?;MicroserviceAuditConnector;Module;GraphiteStartUp;._.Reverse[^.]*""",
    ScoverageKeys.coverageMinimum := 80.00,
    ScoverageKeys.coverageFailOnMinimum := false,
    ScoverageKeys.coverageHighlighting := true,
    Test / parallelExecution := false
  )
}

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val IntegrationTest = config("it") extend Test

lazy val BenchmarkTest = config("benchmark") extend Test

lazy val microservice = (project in file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin)
  .settings(
    name := "gform",
    organization := "uk.gov.hmrc",
    majorVersion := 0,
    PlayKeys.playDefaultPort := 9196,
    scalaSettings,
    defaultSettings(),
    scalafmtOnCompile := true,
    scalaVersion := "2.12.15",
    Test / testOptions := (Test / testOptions).value
      .map {
        // Default Argument added by https://github.com/hmrc/sbt-settings
        // are clashing with munit arguments, so we scope them to ScalaTest instead.
        case sbt.Tests.Argument(None, args) => sbt.Tests.Argument(Some(TestFrameworks.ScalaTest), args)
        case otherwise                      => otherwise
      }
      .toSet
      .toSeq, // get rid of duplicates
    libraryDependencies ++= appDependencies,
    routesImport ++= Seq(
      "uk.gov.hmrc.crypto.Crypted",
      "uk.gov.hmrc.gform.sharedmodel.AffinityGroup",
      "uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress",
      "uk.gov.hmrc.gform.sharedmodel.ValueClassBinder._",
      "uk.gov.hmrc.gform.sharedmodel._",
      "uk.gov.hmrc.gform.sharedmodel.form._",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate._",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._",
      "uk.gov.hmrc.gform.sharedmodel.dblookup._",
      "uk.gov.hmrc.gform.upscan.UpscanReference",
      "uk.gov.hmrc.gform.sharedmodel.sdes.CorrelationId",
      "uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus"
    ),
    resolvers ++= Seq(
      Resolver.jcenterRepo,
      "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",
      "ofsted-notify-java-client" at "https://dl.bintray.com/gov-uk-notify/maven/",
      "HMRC-open-artefacts-maven2" at "https://open.artefacts.tax.service.gov.uk/maven2",
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    ),
    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-Xlint:-missing-interpolator,_",
      "-Yno-adapted-args",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-dead-code",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:higherKinds",
      // silence all warnings on autogenerated files
      "-Wconf:src=routes/.*:silent"
    ),
    Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) },
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
  )
  .configs(IntegrationTest)
  .settings(
    inConfig(IntegrationTest)(Defaults.itSettings),
    inConfig(IntegrationTest)(ScalafmtPlugin.scalafmtConfigSettings),
    IntegrationTest / Keys.fork := false,
    IntegrationTest / unmanagedSourceDirectories := (IntegrationTest / baseDirectory)(base => Seq(base / "it")).value,
    addTestReportOption(IntegrationTest, "int-test-reports"),
    IntegrationTest / parallelExecution := false,
    scalafmtOnCompile := true
  )
  .configs(BenchmarkTest)
  .settings(
    inConfig(BenchmarkTest)(Defaults.testSettings),
    BenchmarkTest / scalaSource := baseDirectory.value / "/benchmark"
  )
