import sbt.{ForkOptions, TestDefinition}
import sbt.Tests.{Group, SubProcess}

object TestPhases {

  def oneForkedJvmPerTest2(tests: Seq[TestDefinition]) =
    tests map {
      test => new Group(test.name, Seq(test), SubProcess(ForkOptions(runJVMOptions = Seq("-Dtest.name=" + test.name))))
    }
}