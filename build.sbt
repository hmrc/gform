import Dependencies.appDependencies
import org.scalafmt.sbt.ScalafmtPlugin
import play.sbt.PlayImport.PlayKeys
import play.sbt.routes.RoutesKeys.routesImport
import sbt.Keys.{ resolvers, _ }
import sbt._
import uk.gov.hmrc.DefaultBuildSettings.{ addTestReportOption, defaultSettings, scalaSettings }
import uk.gov.hmrc.DefaultBuildSettings
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion

ThisBuild / majorVersion := 0

ThisBuild / scalaVersion := "2.13.16"

lazy val scoverageSettings = {
  import scoverage.ScoverageKeys
  Seq(
    ScoverageKeys.coverageExcludedPackages := """uk.gov.hmrc.BuildInfo;._.Routes;._.RoutesPrefix;._Filters?;MicroserviceAuditConnector;Module;GraphiteStartUp;._.Reverse[^.]*""",
    ScoverageKeys.coverageMinimumStmtTotal := 80.00,
    ScoverageKeys.coverageFailOnMinimum := false,
    ScoverageKeys.coverageHighlighting := true,
    Test / parallelExecution := false
  )
}

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val IntegrationTest = config("it") extend Test

lazy val BenchmarkTest = config("benchmark") extend Test

lazy val microservice = (project in file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin, BuildInfoPlugin)
  .settings(
    name := "gform",
    organization := "uk.gov.hmrc",
    PlayKeys.playDefaultPort := 9196,
    scalaSettings,
    defaultSettings(),
    scalafmtOnCompile := true,
    // coverageEnabled := true,
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
      "uk.gov.hmrc.gform.builder.SectionPath",
      "uk.gov.hmrc.gform.sharedmodel.AffinityGroup",
      "uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress",
      "uk.gov.hmrc.gform.sharedmodel.ValueClassBinder._",
      "uk.gov.hmrc.gform.sharedmodel._",
      "uk.gov.hmrc.gform.sharedmodel.form._",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate._",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._",
      "uk.gov.hmrc.gform.sharedmodel.dblookup._",
      "uk.gov.hmrc.gform.history.HistoryId",
      "uk.gov.hmrc.gform.upscan.UpscanReference",
      "uk.gov.hmrc.gform.sharedmodel.sdes._",
      "uk.gov.hmrc.gform.testonly.SnapshotId",
      "uk.gov.hmrc.mongo.workitem.ProcessingStatus",
      "uk.gov.hmrc.gform.translation.audit.TranslationAuditId"
    ),
    resolvers ++= Seq(
      Resolver.jcenterRepo,
      "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",
      "ofsted-notify-java-client" at "https://dl.bintray.com/gov-uk-notify/maven/",
      "HMRC-open-artefacts-maven2" at "https://open.artefacts.tax.service.gov.uk/maven2",
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "jitpack" at "https://jitpack.io" // https://github.com/circe/circe-json-schema?tab=readme-ov-file#setup
    ),
    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-Xlint:-missing-interpolator,_",
      "-Xlint:-byname-implicit",
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
  .configs(BenchmarkTest)
  .settings(
    inConfig(BenchmarkTest)(Defaults.testSettings),
    BenchmarkTest / scalaSource := baseDirectory.value / "/benchmark"
  )
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "uk.gov.hmrc.gform"
  )

lazy val it = project
  .enablePlugins(play.sbt.PlayScala)
  .dependsOn(microservice % "test->test") // the "test->test" allows reusing test code and test dependencies
  .settings(DefaultBuildSettings.itSettings(),
    addTestReportOption(Test, "int-test-reports"),
    scalafmtOnCompile := true,
    run / fork := true, // Enable forking for run
    Test / fork := true, // Enable forking for Test
    Test / javaOptions += "-Dlogger.resource=logback-test.xml"
  )
