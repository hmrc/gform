package uk.gov.hmrc.gform.support

import org.scalatest._
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.time.{Millis, Seconds, Span}

trait ITSpecBase
extends FreeSpecLike
with Matchers
with DiagrammedAssertions
with TryValues
with EitherValues
with OptionValues
with AppendedClues
with ScalaFutures
with Eventually {

  implicit override val patienceConfig = PatienceConfig(timeout = Span(5, Seconds), interval = Span(5, Millis))
}
